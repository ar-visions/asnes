#include <import>

none asnes_init(asnes a) {
    trinity t  = a->t = trinity();
    int width  = 256 * 2;
    int height = 224 * 2;

    a->w = window(
        t, t, title, string("asnes"),
        width, width, height, height);
    
    w->r_background  = target (w, w,
        wscale,         1.0f,
        clear_color,    vec4f(0.0f, 0.1f, 0.2f, 1.0f),
        models,         a());

    initialize(w);
}

none asnes_dealloc(asnes a) {
}

int main(int argc, cstr argv[]) {
    asnes emu = asnes(argv);
    return run(emu);
}