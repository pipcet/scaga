0. Install recent versions of gdb, gcc, emacs, and debug versions of your libraries.
1. configure your project using CFLAGS="-O0 -g3 -fdump-tree-gimple-vops-verbose-raw-lineno" or similar.
2. compile, build, and install your project as normal
3. perl /path/to/scaga/scaga-calls.pl --symbols=1 --executable=exec *.gimple > calls.pl
4. perl /path/to/scaga/scaga-print.pl calls.pl > calls.scaga
5. In Emacs, load scaga-mode.el
6. M-x scaga


SCAGA is a simple (stupid, slow) call graph analysis tool: it
essentially aims to answer the question of whether a function `f`
calls another function `g` directly or indirectly (including through
function pointers).  It does so using GNU utilities: gcc and gdb,
primarily.

Function pointers: SCAGA uses the following tricks to reduce the
number of call chains through function pointers:
 - a manual regular expression rule
 - checking the type of the function pointer
 - checking the "component" of the function pointer: a function pointer that is member `->do_a_hook` of a struct is unlikely to be called by a call to `->do_b_hook()`
 - WIP: devirtualization using link-time optimization

Normal call chains:
 - manual rules
 - LTO to eliminate impossible call chains

LTO call chain elimination:

LTO (link-time optimization) is the GCC approach to optimizing
inter-file interprocedural call chains. The most common case that
interests us is when a function passes a constant argument to a second
function which will prevent that second function from ever calling a
third function, even though there is code in the second function to
call the third function.  For example:

```
int f(void)
{
    return g(1);
}

int g(int x)
{
    if (x != 1)
        return h(x);

    return 0;
}
```

Here, the call chain `f > g > h` is impossible.  LTO allows us to
detect this condition, by asking for `g` to be inlined (but not `f` or
`h`), then inspecting the resulting assembler code for `f` to see
whether a call to `h` remains in it.

The LTO elimination code is currently specific to the x86_64
architecture, since it involves x86 assembler code.

Noreturn optimizations:

Sometimes, functions that do not return aren't relevant to the call
chain analysis. LTO allows fine-grained analysis of which function
calls can conceivably still be followed by a return instruction and
which ones cannot, and options to eliminate functions in the second
category.

Of course, in general it is impossible to know in all cases whether a
function returns or not; however, in practice GCC is very good at
detecting many of the conditions that prevent it from doing so. SCAGA
errs on the side of caution and assumes a function might return if GCC
generates instructions to return from it.

The noreturn code is currently specific to the x86_64 architecture,
since it involves x86 assembler code.

Devirtualization:

Consider this code, from Emacs:

```
static Lisp_Object
x_clipboard_manager_save (Lisp_Object frame)
{
  struct frame *f = XFRAME (frame);
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  Atom data = dpyinfo->Xatom_UTF8_STRING;

  XChangeProperty (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
                   dpyinfo->Xatom_EMACS_TMP,
                   dpyinfo->Xatom_ATOM, 32, PropModeReplace,
                   (unsigned char *) &data, 1);
  x_get_foreign_selection (QCLIPBOARD_MANAGER, QSAVE_TARGETS,
                           Qnil, frame);
  return Qt;
}

void
x_clipboard_manager_save_all (void)
{
    ...
          internal_condition_case_1 (x_clipboard_manager_save, local_frame,
                                     Qt, x_clipboard_manager_error_2);
    ...
}
```

With inlining, LTO correctly recognizes that no general function
pointer is being called: it rejects the call chain
`x_clipboard_manager_save_all > internal_condition_case_1 >
Lisp_Object (*)(Lisp_Object)`. It also generates a special devirtualization rule that looks like this:

```
lto:devirt := x_clipboard_manager_save_all > internal_condition_case_1 > Lisp_Object (*)(Lisp_Object) => x_clipboard_manager_save_all > internal_condition_case_1 > x_clipboard_manager_save
```

If this rule takes effect, it allows only the desired call paths to be
considered.

Recursive functions:

Currently, recursive functions are never inlined.  It would be
desirable to inline them at least once, in particular if it turns out
that the recursive code path isn't actually taken, but there appears
to be no easy way to instruct GCC to do so.

TODO:
 - syntax highlighting in scaga-mode
 - better support for _setjmp()
 - parallelize LTO compilations
