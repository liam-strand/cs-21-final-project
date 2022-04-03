#ifndef SIM_TYPES_INCLUDED
#define SIM_TYPES_INCLUDED

typedef struct Road {
    char *name;
    struct Road *start;
    struct Road *end;
} *Road;

typedef struct Init_State {

    

} *Init_State;

typedef struct Update {
    unsigned car;
    unsigned road;
    unsigned pos;
} *Update;

#endif /* SIM_TYPES_INCLUDED */
