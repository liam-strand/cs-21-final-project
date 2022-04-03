#ifndef PY_COMM_INCLUDED
#define PY_COMM_INCLUDED

extern int read_py(char *pipe, char *buf);
extern int write_py(char *pipe, char *buf, int len);

#endif /* PY_COMM_INCLUDED */
