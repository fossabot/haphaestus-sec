#include <fontconfig/fontconfig.h>
#include <stdlib.h>

struct my_FcCharSetIter {
    FcCharSet *charset;
    FcChar32 map[FC_CHARSET_MAP_SIZE];
    FcChar32 next;
};

struct my_FcCharSetIter *my_FcCharSetIterCreate(FcCharSet *a) {
    struct my_FcCharSetIter *self = malloc(sizeof(struct my_FcCharSetIter));
    self->charset = FcCharSetCopy(a);
    return self;
}

void my_FcCharSetIterDestroy(struct my_FcCharSetIter *self) {
    FcCharSetDestroy(self->charset);
    free(self);
}

FcChar32 my_FcCharSetIterStart(struct my_FcCharSetIter *self) {
    return FcCharSetFirstPage(self->charset, self->map, &self->next);
}

FcChar32 my_FcCharSetIterNext(struct my_FcCharSetIter *self) {
    return FcCharSetNextPage(self->charset, self->map, &self->next);
}

FcBool my_FcCharSetIterDone(FcChar32 chr) {
    return chr == FC_CHARSET_DONE;
}
