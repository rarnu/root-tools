{$mode objfpc}

{$ifdef bsd}
  {$linklib c}
{$endif}

unit sqlite;

interface

{
  Automatically converted by H2Pas 0.99.15 from sqlite.h
  The following command line parameters were used:
    -S
    -D
    -p
    -l
    sqlite
    sqlite.h
}

{$PACKRECORDS C}

const
  External_library='sqlite';
 
  SQLITE_ISO8859 = 1;//?
  
  //sqlite_exec and sqlite_step return values
  SQLITE_OK         = 0;
  SQLITE_ERROR      = 1;
  SQLITE_INTERNAL   = 2;
  SQLITE_PERM       = 3;
  SQLITE_ABORT      = 4;
  SQLITE_BUSY       = 5;
  SQLITE_LOCKED     = 6;
  SQLITE_NOMEM      = 7;
  SQLITE_READONLY   = 8;
  SQLITE_INTERRUPT  = 9;
  SQLITE_IOERR      = 10;
  SQLITE_CORRUPT    = 11;
  SQLITE_NOTFOUND   = 12;
  SQLITE_FULL       = 13;
  SQLITE_CANTOPEN   = 14;
  SQLITE_PROTOCOL   = 15;
  SQLITE_EMPTY      = 16;
  SQLITE_SCHEMA     = 17;
  SQLITE_TOOBIG     = 18;
  SQLITE_CONSTRAINT = 19;
  SQLITE_MISMATCH   = 20;
  SQLITE_MISUSE     = 21;
  SQLITE_NOLFS      = 22;
  SQLITE_AUTH       = 23;
  SQLITE_FORMAT     = 24;
  SQLITE_RANGE      = 25;
  SQLITE_ROW        = 100;
  SQLITE_DONE       = 101;

  // values used in sqlite_set_authorizer to define what operations authorize
  SQLITE_COPY                = 0;
  SQLITE_CREATE_INDEX        = 1;
  SQLITE_CREATE_TABLE        = 2;
  SQLITE_CREATE_TEMP_INDEX   = 3;
  SQLITE_CREATE_TEMP_TABLE   = 4;
  SQLITE_CREATE_TEMP_TRIGGER = 5;
  SQLITE_CREATE_TEMP_VIEW    = 6;
  SQLITE_CREATE_TRIGGER      = 7;
  SQLITE_CREATE_VIEW         = 8;
  SQLITE_DELETE              = 9;
  SQLITE_DROP_INDEX          = 10;
  SQLITE_DROP_TABLE          = 11;
  SQLITE_DROP_TEMP_INDEX     = 12;
  SQLITE_DROP_TEMP_TABLE     = 13;
  SQLITE_DROP_TEMP_TRIGGER   = 14;
  SQLITE_DROP_TEMP_VIEW      = 15;
  SQLITE_DROP_TRIGGER        = 16;
  SQLITE_DROP_VIEW           = 17;
  SQLITE_INSERT              = 18;
  SQLITE_PRAGMA              = 19;
  SQLITE_READ                = 20;
  SQLITE_SELECT              = 21;
  SQLITE_TRANSACTION         = 22;
  SQLITE_UPDATE              =  23;
  
  //Return values of the authorizer function
  SQLITE_DENY                = 1;
  SQLITE_IGNORE              = 2;

  SQLITE_NUMERIC = -1;
  SQLITE_TEXT    = -2;
  SQLITE_ARGS    = -3;

Type
  Psqlite = Pointer;
  Psqlite_vm = Pointer;
  PPsqlite_vm = ^Psqlite_vm;
  Psqlite_func = Pointer;

  // Procedural types used in functions.

  sqlite_callback = function (_para1:pointer; _para2:longint; _para3:PPchar; _para4:PPchar):longint;cdecl;
  sqlite_trace_func = procedure (_para1:pointer; _para2:Pchar);cdecl;
  sqlite_create_func = procedure (_para1:Psqlite_func; _para2:longint; _para3:PPchar);cdecl;
  sqlite_handler = function (_para1:pointer; _para2:Pchar; _para3:longint):longint;cdecl;
  sqlite_step_func = procedure (_para1:Psqlite_func; _para2:longint; _para3:PPchar);cdecl;
  sqlite_finalize_func = procedure (_para1:Psqlite_func);cdecl;
  sqlite_authorize_func = function (_para1:pointer; _para2:longint; _para3, _para4,_para5,_para6:Pchar):longint;cdecl;

  function sqlite_create_function(_para1:Psqlite; zName:Pchar; nArg:longint; xFunc:sqlite_create_func; pUserData:pointer):longint;cdecl;external External_library name 'sqlite_create_function';
  function sqlite_open(filename:Pchar; mode:longint; errmsg:PPchar):Psqlite;cdecl;external External_library name 'sqlite_open';
  procedure sqlite_close(_para1:Psqlite);cdecl;external External_library name 'sqlite_close';
  function sqlite_exec(_para1:Psqlite; sql:Pchar; _para3:sqlite_callback; _para4:pointer; errmsg:PPchar):longint;cdecl;external External_library name 'sqlite_exec';
  function sqlite_last_insert_rowid(_para1:Psqlite):longint;cdecl;external External_library name 'sqlite_last_insert_rowid';
  function sqlite_changes(_para1:Psqlite):longint;cdecl;external External_library name 'sqlite_changes';
  function sqlite_error_string(_para1:longint):Pchar;cdecl;external External_library name 'sqlite_error_string';
  procedure do_sqlite_interrupt(_para1:Psqlite);cdecl;external External_library name 'sqlite_interrupt';
  function sqlite_complete(sql:Pchar):longint;cdecl;external External_library name 'sqlite_complete';
  procedure sqlite_busy_handler(_para1:Psqlite; _para2:sqlite_handler; _para3:pointer);cdecl;external External_library name 'sqlite_busy_handler';
  procedure sqlite_busy_timeout(_para1:Psqlite; ms:longint);cdecl;external External_library name 'sqlite_busy_timeout';
  function sqlite_get_table(_para1:Psqlite; sql:Pchar; resultp:PPPchar; nrow:Plongint; ncolumn:Plongint;
             errmsg:PPchar):longint;cdecl;external External_library name 'sqlite_get_table';
  procedure sqlite_free_table(result:PPchar);cdecl;external External_library name 'sqlite_free_table';
  function sqlite_exec_printf(_para1:Psqlite; sqlFormat:Pchar; _para3:sqlite_callback; _para4:pointer; errmsg:PPchar;
             args:array of const):longint;cdecl;external External_library name 'sqlite_exec_printf';
  function sqlite_exec_printf(_para1:Psqlite; sqlFormat:Pchar; _para3:sqlite_callback; _para4:pointer; errmsg:PPchar):longint;cdecl;varargs;external External_library name 'sqlite_exec_printf';
  function sqlite_exec_vprintf(_para1:Psqlite; sqlFormat:Pchar; _para3:sqlite_callback; _para4:pointer; errmsg:PPchar;
             ap:array of const):longint;cdecl;external External_library name 'sqlite_exec_vprintf';
  function sqlite_get_table_printf(_para1:Psqlite; sqlFormat:Pchar; resultp:PPPchar; nrow:Plongint; ncolumn:Plongint;
             errmsg:PPchar; args:array of const):longint;cdecl;external External_library name 'sqlite_get_table_printf';
  function sqlite_get_table_printf(_para1:Psqlite; sqlFormat:Pchar; resultp:PPPchar; nrow:Plongint; ncolumn:Plongint;
             errmsg:PPchar):longint;cdecl;varargs;external External_library name 'sqlite_get_table_printf';
  function sqlite_get_table_vprintf(_para1:Psqlite; sqlFormat:Pchar; resultp:PPPchar; nrow:Plongint; ncolumn:Plongint;
             errmsg:PPchar; ap:array of const):longint;cdecl;external External_library name 'sqlite_get_table_vprintf';
  function sqlite_mprintf(_para1:Pchar; args:array of const):Pchar;cdecl;external External_library name 'sqlite_mprintf';
  function sqlite_mprintf(_para1:Pchar):Pchar;cdecl;varargs;external External_library name 'sqlite_mprintf';
  procedure sqlite_freemem(p:pointer);cdecl;external External_library name 'sqlite_freemem';
  function sqlite_libversion:Pchar;cdecl;external External_library name 'sqlite_libversion';
  function sqlite_libencoding:Pchar;cdecl;external External_library name 'sqlite_libencoding';
  function sqlite_create_aggregate(_para1:Psqlite; zName:Pchar; nArg:longint; xStep:sqlite_step_func ; xFinalize:sqlite_finalize_func;
             pUserData:pointer):longint;cdecl;external External_library name 'sqlite_create_aggregate';
  function sqlite_function_type(db:Psqlite; zName:Pchar; datatype:longint):longint;cdecl;external External_library name 'sqlite_function_type';
  function sqlite_set_result_string(_para1:Psqlite_func; _para2:Pchar; _para3:longint):Pchar;cdecl;external External_library name 'sqlite_set_result_string';
  procedure sqlite_set_result_int(_para1:Psqlite_func; _para2:longint);cdecl;external External_library name 'sqlite_set_result_int';
  procedure sqlite_set_result_double(_para1:Psqlite_func; _para2:double);cdecl;external External_library name 'sqlite_set_result_double';
  procedure sqlite_set_result_error(_para1:Psqlite_func; _para2:Pchar; _para3:longint);cdecl;external External_library name 'sqlite_set_result_error';
  function sqlite_user_data(_para1:Psqlite_func):pointer;cdecl;external External_library name 'sqlite_user_data';
  function sqlite_aggregate_context(_para1:Psqlite_func; nBytes:longint):pointer;cdecl;external External_library name 'sqlite_aggregate_context';
  function sqlite_aggregate_count(_para1:Psqlite_func):longint;cdecl;external External_library name 'sqlite_aggregate_count';
  function sqlite_set_authorizer(_para1:Psqlite; xAuth:sqlite_authorize_func ; pUserData:pointer):longint;cdecl;external External_library name 'sqlite_set_authorizer';
  function sqlite_trace(_para1:Psqlite; xTrace:sqlite_trace_func; _para3:pointer):pointer;cdecl;external External_library name 'sqlite_trace';
  function sqlite_compile(db:Psqlite; zSql:Pchar; pzTail:PPchar; ppVm:PPsqlite_vm; pzErrmsg:PPchar):longint;cdecl;external External_library name 'sqlite_compile';
  function sqlite_step(pVm:Psqlite_vm; pN:Plongint; pazValue:PPPchar; pazColName:PPPchar):longint;cdecl;external External_library name 'sqlite_step';
  function sqlite_finalize(_para1:Psqlite_vm; pzErrMsg:PPchar):longint;cdecl;external External_library name 'sqlite_finalize';
  
  //Use functions instead of external variables to retrieve version and encoding info
  function sqlite_version: PChar external External_library name 'sqlite_libversion';
  function sqlite_encoding: PChar external External_library name 'sqlite_libencoding';
  
implementation

end.
