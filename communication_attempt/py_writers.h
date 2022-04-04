#ifndef PY_WRITERS_INCLUDED
#define PY_WRITERS_INCLUDED

extern void Py_write_state(char *pipe, char *buf);
extern void Py_write_update(char *pipe, char *buf, int len);

#endif /* PY_WRITERS_INCLUDED */
