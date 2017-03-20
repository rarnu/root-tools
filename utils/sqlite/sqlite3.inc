{$mode objfpc}{$h+}

{$ifdef BSD}
  {$linklib c}
  {$linklib pthread}
{$endif}

{$packrecords C}

{.$DEFINE SQLITE_OBSOLETE}

interface

uses
  ctypes,
{$ifdef LOAD_DYNAMICALLY}
  SysUtils, DynLibs;
{$else}
  DynLibs;

  {$ifdef darwin}
    {$linklib sqlite3}
  {$endif}
{$endif}

const
{$IFDEF WINDOWS}
  Sqlite3Lib = 'sqlite3.dll';
{$else}
  {$IFDEF ANDROID}
  Sqlite3Lib = 'libsqlite.'+sharedsuffix;
  {$ELSE}
  Sqlite3Lib = 'libsqlite3.'+sharedsuffix;
  {$ENDIF}
{$endif}

{$IFDEF LOAD_DYNAMICALLY}
  {$DEFINE D}
{$ELSE}
  {$DEFINE S}
{$ENDIF}

{
  Header converted from Sqlite version 3.14.2
}

//SQLITE_EXTERN const char sqlite3_version[];
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_libversion{$IFDEF D}: function{$ENDIF}(): pansichar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_libversion_number{$IFDEF D}: function{$ENDIF}(): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
function sqlite3_version(): pansichar;

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_threadsafe{$IFDEF D}: function{$ENDIF}(): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

type
  ppsqlite3 = ^psqlite3;
  psqlite3 = ^_sqlite3;
  _sqlite3 = record end;

  
type
  sqlite3_destructor_type = procedure(user: pointer); cdecl;

const
  SQLITE_STATIC      = sqlite3_destructor_type(nil);
  SQLITE_TRANSIENT   = pointer(-1); //sqlite3_destructor_type(-1);
  

type
  psqlite_int64 = ^sqlite_int64;
  sqlite_int64 = Int64;

  psqlite_uint64 = ^sqlite_uint64;
  sqlite_uint64 = QWord;

  psqlite3_int64 = ^sqlite3_int64;
  sqlite3_int64 = sqlite_int64;

  psqlite3_uint64 = ^sqlite3_uint64;
  sqlite3_uint64 = sqlite_uint64;

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_close{$IFDEF D}: function{$ENDIF}(ref: psqlite3): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_close_v2{$IFDEF D}: function{$ENDIF}(ref: psqlite3): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

type
  sqlite3_callback = function(user: pointer; cols: cint; values, name: ppansichar): cint; cdecl;

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_exec{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;                              (* An open database *)
  sql: pansichar;                            (* SQL to be evaluted *)
  cb: sqlite3_callback;                      (* Callback function *)
  user: pointer;                             (* 1st argument to callback *)
  errmsg: ppansichar                         (* Error msg written here *)
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

const
  SQLITE_OK         =  0;   (* Successful result *)
(* beginning-of-error-codes *)
  SQLITE_ERROR      =  1;   (* SQL error or missing database *)
  SQLITE_INTERNAL   =  2;   (* Internal logic error in SQLite *)
  SQLITE_PERM       =  3;   (* Access permission denied *)
  SQLITE_ABORT      =  4;   (* Callback routine requested an abort *)
  SQLITE_BUSY       =  5;   (* The database file is locked *)
  SQLITE_LOCKED     =  6;   (* A table in the database is locked *)
  SQLITE_NOMEM      =  7;   (* A malloc() failed *)
  SQLITE_READONLY   =  8;   (* Attempt to write a readonly database *)
  SQLITE_INTERRUPT  =  9;   (* Operation terminated by sqlite3_interrupt()*)
  SQLITE_IOERR      = 10;   (* Some kind of disk I/O error occurred *)
  SQLITE_CORRUPT    = 11;   (* The database disk image is malformed *)
  SQLITE_NOTFOUND   = 12;   (* NOT USED. Table or record not found *)
  SQLITE_FULL       = 13;   (* Insertion failed because database is full *)
  SQLITE_CANTOPEN   = 14;   (* Unable to open the database file *)
  SQLITE_PROTOCOL   = 15;   (* NOT USED. Database lock protocol error *)
  SQLITE_EMPTY      = 16;   (* Database is empty *)
  SQLITE_SCHEMA     = 17;   (* The database schema changed *)
  SQLITE_TOOBIG     = 18;   (* String or BLOB exceeds size limit *)
  SQLITE_CONSTRAINT = 19;   (* Abort due to constraint violation *)
  SQLITE_MISMATCH   = 20;   (* Data type mismatch *)
  SQLITE_MISUSE     = 21;   (* Library used incorrectly *)
  SQLITE_NOLFS      = 22;   (* Uses OS features not supported on host *)
  SQLITE_AUTH       = 23;   (* Authorization denied *)
  SQLITE_FORMAT     = 24;   (* Auxiliary database format error *)
  SQLITE_RANGE      = 25;   (* 2nd parameter to sqlite3_bind out of range *)
  SQLITE_NOTADB     = 26;   (* File opened that is not a database file *)
  SQLITE_NOTICE     = 27;   (* Notifications from sqlite3_log() *)
  SQLITE_WARNING    = 28;   (* Warnings from sqlite3_log() *)
  SQLITE_ROW        = 100;  (* sqlite3_step() has another row ready *)
  SQLITE_DONE       = 101;  (* sqlite3_step() has finished executing *)

  SQLITE_IOERR_READ          = (SQLITE_IOERR or (1 shl 8));
  SQLITE_IOERR_SHORT_READ    = (SQLITE_IOERR or (2 shl 8));
  SQLITE_IOERR_WRITE         = (SQLITE_IOERR or (3 shl 8));
  SQLITE_IOERR_FSYNC         = (SQLITE_IOERR or (4 shl 8));
  SQLITE_IOERR_DIR_FSYNC     = (SQLITE_IOERR or (5 shl 8));
  SQLITE_IOERR_TRUNCATE      = (SQLITE_IOERR or (6 shl 8));
  SQLITE_IOERR_FSTAT         = (SQLITE_IOERR or (7 shl 8));
  SQLITE_IOERR_UNLOCK        = (SQLITE_IOERR or (8 shl 8));
  SQLITE_IOERR_RDLOCK        = (SQLITE_IOERR or (9 shl 8));
  SQLITE_IOERR_DELETE        = (SQLITE_IOERR or (10 shl 8));
  SQLITE_IOERR_BLOCKED       = (SQLITE_IOERR or (11 shl 8));
  SQLITE_IOERR_NOMEM         = (SQLITE_IOERR or (12 shl 8));
  SQLITE_IOERR_ACCESS            = (SQLITE_IOERR or (13 shl 8));
  SQLITE_IOERR_CHECKRESERVEDLOCK = (SQLITE_IOERR or (14 shl 8));
  SQLITE_IOERR_LOCK              = (SQLITE_IOERR or (15 shl 8));
  SQLITE_IOERR_CLOSE             = (SQLITE_IOERR or (16 shl 8));
  SQLITE_IOERR_DIR_CLOSE         = (SQLITE_IOERR or (17 shl 8));
  SQLITE_IOERR_SHMOPEN           = (SQLITE_IOERR or (18 shl 8));
  SQLITE_IOERR_SHMSIZE           = (SQLITE_IOERR or (19 shl 8));
  SQLITE_IOERR_SHMLOCK           = (SQLITE_IOERR or (20 shl 8));
  SQLITE_IOERR_SHMMAP            = (SQLITE_IOERR or (21 shl 8));
  SQLITE_IOERR_SEEK              = (SQLITE_IOERR or (22 shl 8));
  SQLITE_IOERR_DELETE_NOENT      = (SQLITE_IOERR or (23 shl 8));
  SQLITE_IOERR_MMAP              = (SQLITE_IOERR or (24 shl 8));
  SQLITE_IOERR_GETTEMPPATH       = (SQLITE_IOERR or (25 shl 8));
  SQLITE_IOERR_CONVPATH          = (SQLITE_IOERR or (26 shl 8));
  SQLITE_IOERR_VNODE             = (SQLITE_IOERR or (27 shl 8));
  SQLITE_IOERR_AUTH              = (SQLITE_IOERR or (28 shl 8));
  SQLITE_LOCKED_SHAREDCACHE      = (SQLITE_LOCKED or  (1 shl 8));
  SQLITE_BUSY_RECOVERY           = (SQLITE_BUSY   or  (1 shl 8));
  SQLITE_BUSY_SNAPSHOT           = (SQLITE_BUSY   or  (2 shl 8));
  SQLITE_CANTOPEN_NOTEMPDIR      = (SQLITE_CANTOPEN or (1 shl 8));
  SQLITE_CANTOPEN_ISDIR          = (SQLITE_CANTOPEN or (2 shl 8));
  SQLITE_CANTOPEN_FULLPATH       = (SQLITE_CANTOPEN or (3 shl 8));
  SQLITE_CANTOPEN_CONVPATH       = (SQLITE_CANTOPEN or (4 shl 8));
  SQLITE_CORRUPT_VTAB            = (SQLITE_CORRUPT or (1 shl 8));
  SQLITE_READONLY_RECOVERY       = (SQLITE_READONLY or (1 shl 8));
  SQLITE_READONLY_CANTLOCK       = (SQLITE_READONLY or (2 shl 8));
  SQLITE_READONLY_ROLLBACK       = (SQLITE_READONLY or (3 shl 8));
  SQLITE_READONLY_DBMOVED        = (SQLITE_READONLY or (4 shl 8));
  SQLITE_ABORT_ROLLBACK          = (SQLITE_ABORT or (2 shl 8));
  SQLITE_CONSTRAINT_CHECK        = (SQLITE_CONSTRAINT or (1 shl 8));
  SQLITE_CONSTRAINT_COMMITHOOK   = (SQLITE_CONSTRAINT or (2 shl 8));
  SQLITE_CONSTRAINT_FOREIGNKEY   = (SQLITE_CONSTRAINT or (3 shl 8));
  SQLITE_CONSTRAINT_FUNCTION     = (SQLITE_CONSTRAINT or (4 shl 8));
  SQLITE_CONSTRAINT_NOTNULL      = (SQLITE_CONSTRAINT or (5 shl 8));
  SQLITE_CONSTRAINT_PRIMARYKEY   = (SQLITE_CONSTRAINT or (6 shl 8));
  SQLITE_CONSTRAINT_TRIGGER      = (SQLITE_CONSTRAINT or (7 shl 8));
  SQLITE_CONSTRAINT_UNIQUE       = (SQLITE_CONSTRAINT or (8 shl 8));
  SQLITE_CONSTRAINT_VTAB         = (SQLITE_CONSTRAINT or (9 shl 8));
  SQLITE_CONSTRAINT_ROWID        = (SQLITE_CONSTRAINT or (10 shl 8));
  SQLITE_NOTICE_RECOVER_WAL      = (SQLITE_NOTICE or (1 shl 8));
  SQLITE_NOTICE_RECOVER_ROLLBACK = (SQLITE_NOTICE or (2 shl 8));
  SQLITE_WARNING_AUTOINDEX       = (SQLITE_WARNING or (1 shl 8));
  SQLITE_AUTH_USER               = (SQLITE_AUTH or (1 shl 8));
  SQLITE_OK_LOAD_PERMANENTLY     = (SQLITE_OK or (1 shl 8));

  SQLITE_OPEN_READONLY         = $00000001;
  SQLITE_OPEN_READWRITE        = $00000002;
  SQLITE_OPEN_CREATE           = $00000004;
  SQLITE_OPEN_DELETEONCLOSE    = $00000008;
  SQLITE_OPEN_EXCLUSIVE        = $00000010;
  SQLITE_OPEN_AUTOPROXY        = $00000020;  (* VFS only *)
  SQLITE_OPEN_URI              = $00000040;  (* Ok for sqlite3_open_v2() *)
  SQLITE_OPEN_MEMORY           = $00000080;  (* Ok for sqlite3_open_v2() *)
  SQLITE_OPEN_MAIN_DB          = $00000100;
  SQLITE_OPEN_TEMP_DB          = $00000200;
  SQLITE_OPEN_TRANSIENT_DB     = $00000400;
  SQLITE_OPEN_MAIN_JOURNAL     = $00000800;
  SQLITE_OPEN_TEMP_JOURNAL     = $00001000;
  SQLITE_OPEN_SUBJOURNAL       = $00002000;
  SQLITE_OPEN_MASTER_JOURNAL   = $00004000;
  SQLITE_OPEN_NOMUTEX          = $00008000;
  SQLITE_OPEN_FULLMUTEX        = $00010000;
  SQLITE_OPEN_SHAREDCACHE      = $00020000;
  SQLITE_OPEN_PRIVATECACHE     = $00040000;
  SQLITE_OPEN_WAL              = $00080000;


  SQLITE_IOCAP_ATOMIC          = $00000001;
  SQLITE_IOCAP_ATOMIC512       = $00000002;
  SQLITE_IOCAP_ATOMIC1K        = $00000004;
  SQLITE_IOCAP_ATOMIC2K        = $00000008;
  SQLITE_IOCAP_ATOMIC4K        = $00000010;
  SQLITE_IOCAP_ATOMIC8K        = $00000020;
  SQLITE_IOCAP_ATOMIC16K       = $00000040;
  SQLITE_IOCAP_ATOMIC32K       = $00000080;
  SQLITE_IOCAP_ATOMIC64K       = $00000100;
  SQLITE_IOCAP_SAFE_APPEND     = $00000200;
  SQLITE_IOCAP_SEQUENTIAL      = $00000400;
  SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN  = $00000800;
  SQLITE_IOCAP_POWERSAFE_OVERWRITE    = $00001000;
  SQLITE_IOCAP_IMMUTABLE              = $00002000;

  SQLITE_LOCK_NONE          = 0;
  SQLITE_LOCK_SHARED        = 1;
  SQLITE_LOCK_RESERVED      = 2;
  SQLITE_LOCK_PENDING       = 3;
  SQLITE_LOCK_EXCLUSIVE     = 4;

  SQLITE_SYNC_NORMAL        = $00002;
  SQLITE_SYNC_FULL          = $00003;
  SQLITE_SYNC_DATAONLY      = $00010;


type
  psqlite3_io_methods = ^sqlite3_io_methods;

  psqlite3_file = ^sqlite3_file;
  sqlite3_file = record
    pMethods: psqlite3_io_methods;  (* Methods for an open file *)
  end;

  sqlite3_io_methods = record
     iVersion          : cint;
     Close             : function(f: psqlite3_file): cint; stdcall;
     Read              : function(f: psqlite3_file; addr: pointer; iAmt: cint; iOfst: sqlite3_int64): cint; stdcall;
     Write             : function(f: psqlite3_file; size: sqlite3_int64): cint; stdcall;
     Truncate          : function(f: psqlite3_file; size: sqlite3_int64): cint; stdcall;
     Sync              : function(f: psqlite3_file; flags: cint): cint; stdcall;
     FileSize          : function(f: psqlite3_file; pSize: psqlite3_int64): cint; stdcall;
     Lock              : function(f: psqlite3_file; flags: cint): cint; stdcall;
     Unlock            : function(f: psqlite3_file; flags: cint): cint; stdcall;
     CheckReservedLock : function(f: psqlite3_file): cint; stdcall;
     FileControl       : function(f: psqlite3_file; op: cint; pArg: pointer): cint; stdcall;
     SectorSize        : function(f: psqlite3_file): cint; stdcall;
     DeviceCharacteristics: function(f: psqlite3_file): cint; stdcall;
     xShmMap : function(f : psqlite3_file; iPg: cint; pgsz: cint; volatile : pointer) : cint;stdcall;
     xShmLock : function(f : psqlite3_file; offset: cint; n : cint; flags : cint) : cint; stdcall;
     xShmBarrier  : procedure (f : psqlite3_file); stdcall;
     xShmUnmap : function(f : psqlite3_file; deleteFlag : cint) : cint; stdcall;
     (* Methods above are valid for version 2 *)
     xFetch : function(f: psqlite3_file; iOfst: sqlite3_int64; iAmt: cint; pp: PPointer) : cint; stdcall;
     xUnfetch : function(f: psqlite3_file; iOfst: sqlite3_int64; p: Pointer) : cint; stdcall;
     (* Methods above are valid for version 3 *)
     (* Additional methods may be added in future releases *)
  end;

const
  SQLITE_FCNTL_LOCKSTATE           = 1;
  SQLITE_FCNTL_GET_LOCKPROXYFILE   = 2;
  SQLITE_FCNTL_SET_LOCKPROXYFILE   = 3;
  SQLITE_FCNTL_LAST_ERRNO          = 4;
  SQLITE_FCNTL_SIZE_HINT           = 5;
  SQLITE_FCNTL_CHUNK_SIZE          = 6;
  SQLITE_FCNTL_FILE_POINTER        = 7;
  SQLITE_FCNTL_SYNC_OMITTED        = 8;
  SQLITE_FCNTL_WIN32_AV_RETRY      = 9;
  SQLITE_FCNTL_PERSIST_WAL         = 10;
  SQLITE_FCNTL_OVERWRITE           = 11;
  SQLITE_FCNTL_VFSNAME             = 12;
  SQLITE_FCNTL_POWERSAFE_OVERWRITE = 13;
  SQLITE_FCNTL_PRAGMA              = 14;
  SQLITE_FCNTL_BUSYHANDLER         = 15;
  SQLITE_FCNTL_TEMPFILENAME        = 16;
  SQLITE_FCNTL_MMAP_SIZE           = 18;
  SQLITE_FCNTL_TRACE               = 19;
  SQLITE_FCNTL_HAS_MOVED           = 20;
  SQLITE_FCNTL_SYNC                = 21;
  SQLITE_FCNTL_COMMIT_PHASETWO     = 22;
  SQLITE_FCNTL_WIN32_SET_HANDLE    = 23;
  SQLITE_FCNTL_WAL_BLOCK           = 24;
  SQLITE_FCNTL_ZIPVFS              = 25;
  SQLITE_FCNTL_RBU                 = 26;
  SQLITE_FCNTL_VFS_POINTER         = 27;
  SQLITE_FCNTL_JOURNAL_POINTER     = 28;

  (* deprecated names *)
  SQLITE_GET_LOCKPROXYFILE    = SQLITE_FCNTL_GET_LOCKPROXYFILE;
  SQLITE_SET_LOCKPROXYFILE    = SQLITE_FCNTL_SET_LOCKPROXYFILE;
  SQLITE_LAST_ERRNO           = SQLITE_FCNTL_LAST_ERRNO;

type
  psqlite3_mutex = ^sqlite3_mutex;
  sqlite3_mutex = record end;

type
  psqlite3_vfs = ^sqlite3_vfs;
  sqlite3_vfs = record
    iVersion     : cint;          (* Structure version number *)
    szOsFile     : cint;          (* Size of subclassed sqlite3_file *)
    mxPathname   : cint;          (* Maximum file pathname length *)
    pNext        : psqlite3_vfs;  (* Next registered VFS *)
    zName        : pansichar;     (* Name of this virtual file system *)
    pAppData     : ppointer;      (* Pointer to application-specific *)
    Open         : function(vfs: psqlite3_vfs; zName: pansichar; f: psqlite3_file; flags: cint; pOutFlags: pcint): cint; cdecl;
    Delete       : function(vfs: psqlite3_vfs; zName: pansichar; syncDir: cint): cint; cdecl;
    Access       : function(vfs: psqlite3_vfs; zName: pansichar; flags: cint): cint; cdecl;
    FullPathname : function(vfs: psqlite3_vfs; zName: pansichar; nOut: cint; zOut: pansichar): cint; cdecl;
    DlOpen       : function(vfs: psqlite3_vfs; zFilename: pansichar): pointer; cdecl;
    DlError      : procedure(vfs: psqlite3_vfs; nByte: cint; zErrMsg: pansichar); cdecl;
    DlSym        : function(vfs: psqlite3_vfs; addr: pointer; zSymbol: pansichar): pointer; cdecl;
    DlClose      : procedure(vfs: psqlite3_vfs; addr: pointer); cdecl;
    Randomness   : function(vfs: psqlite3_vfs; nByte: cint; zOut: pansichar): cint; cdecl;
    Sleep        : function(vfs: psqlite3_vfs; microseconds: cint): cint; cdecl;    
    CurrentTime  : function(vfs: psqlite3_vfs; time: pcdouble): cint; cdecl;
    GetLastError : function(vfs: psqlite3_vfs; code: cint; msg: pansichar): cint; cdecl;
    CurrentTimeInt64 : function(vfs: psqlite3_vfs; time: psqlite3_int64): cint; cdecl; 	
    xSetSystemCall : function(vfs: psqlite3_vfs; zName: pansichar; sqlite3_syscall_ptr : pointer) : cint; cdecl;
    xGetSystemCall : function(vfs: psqlite3_vfs; zName: pansichar) : pointer; cdecl;
    xNextSystemCall : function(vfs: psqlite3_vfs; zName: pansichar) : pansichar; cdecl;
  end;

const
  SQLITE_ACCESS_EXISTS    = 0;
  SQLITE_ACCESS_READWRITE = 1;
  SQLITE_ACCESS_READ      = 2;
  SQLITE_SHM_UNLOCK       = 1;
  SQLITE_SHM_LOCK         = 2;
  SQLITE_SHM_SHARED       = 4;
  SQLITE_SHM_EXCLUSIVE    = 8;
  SQLITE_SHM_NLOCK        = 8;

{$IFDEF S}
  function sqlite3_initialize : cint;cdecl; external Sqlite3Lib;
  function sqlite3_shutdown : cint;cdecl; external Sqlite3Lib;
  function sqlite3_os_init : cint;cdecl; external Sqlite3Lib;
  function sqlite3_os_end : cint;cdecl; external Sqlite3Lib;
{$endif}  
{$ifdef D}
Var
  sqlite3_initialize,
  sqlite3_shutdown,
  sqlite3_os_init,
  sqlite3_os_end : Function : cint; cdecl;
{$endif}

{$IFDEF S}
 function sqlite3_config (a : cint): cint;cdecl;varargs; external Sqlite3Lib;
{$ENDIF}
{$IFDEF D}
var 
  sqlite3_config : function (a : cint): cint;cdecl;varargs;
{$ENDIF}

{$IFDEF S}
 function sqlite3_db_config (f : psqlite3; op : cint):cint;cdecl;varargs;external Sqlite3Lib;
{$ENDIF}
{$IFDEF D}
var 
 sqlite3_db_config : function (f : psqlite3; op : cint): cint;cdecl;varargs;
{$ENDIF}

Type
sqlite3_mem_methods = record
  xMalloc : function(size : cint) : pointer;cdecl;
  xFree : procedure(p : pointer); cdecl;
  xRealloc : function(p : pointer;size : cint) : pointer;cdecl;
  xSize : function(p : pointer) : cint; cdecl;
  xRoundup : function(size : cint) : cint; cdecl;
  xInit : function(): cint; cdecl;
  xShutdown : procedure(p : pointer); cdecl;
  pAppData: pointer;
end;
Tsqlite3_mem_methods = sqlite3_mem_methods;

Const
  SQLITE_CONFIG_SINGLETHREAD = 1;
  SQLITE_CONFIG_MULTITHREAD  = 2;
  SQLITE_CONFIG_SERIALIZED   = 3;
  SQLITE_CONFIG_MALLOC       = 4;
  SQLITE_CONFIG_GETMALLOC    = 5;
  SQLITE_CONFIG_SCRATCH      = 6;
  SQLITE_CONFIG_PAGECACHE    = 7;
  SQLITE_CONFIG_HEAP         = 8;
  SQLITE_CONFIG_MEMSTATUS    = 9;
  SQLITE_CONFIG_MUTEX        = 10;
  SQLITE_CONFIG_GETMUTEX     = 11;
  SQLITE_CONFIG_LOOKASIDE    = 13;
  SQLITE_CONFIG_PCACHE       = 14;
  SQLITE_CONFIG_GETPCACHE    = 15;
  SQLITE_CONFIG_LOG          = 16;
  SQLITE_CONFIG_URI          = 17;
  SQLITE_CONFIG_PCACHE2      = 18;  (* sqlite3_pcache_methods2* *)
  SQLITE_CONFIG_GETPCACHE2   = 19;  (* sqlite3_pcache_methods2* *)
  SQLITE_CONFIG_COVERING_INDEX_SCAN = 20;  (* int *)
  SQLITE_CONFIG_SQLLOG              = 21;  (* xSqllog, void* *)
  SQLITE_CONFIG_MMAP_SIZE           = 22;  (* sqlite3_int64, sqlite3_int64 *)
  SQLITE_CONFIG_WIN32_HEAPSIZE      = 23;  (* int nByte *)
  SQLITE_CONFIG_PCACHE_HDRSZ        = 24;  (* int *psz *)
  SQLITE_CONFIG_PMASZ               = 25;  (* unsigned int szPma *)
  SQLITE_CONFIG_STMTJRNL_SPILL      = 26;  (* int nByte *)

  SQLITE_DBCONFIG_LOOKASIDE      = 1001;
  SQLITE_DBCONFIG_ENABLE_FKEY    = 1002;
  SQLITE_DBCONFIG_ENABLE_TRIGGER = 1003;
  SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER = 1004;
  SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION = 1005;

  
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_extended_result_codes{$IFDEF D}: function{$ENDIF}(db: psqlite3; onoff: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_last_insert_rowid{$IFDEF D}: function{$ENDIF}(db: psqlite3): sqlite3_int64; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_changes{$IFDEF D}: function{$ENDIF}(db: psqlite3): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_total_changes{$IFDEF D}: function{$ENDIF}(db: psqlite3): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_interrupt{$IFDEF D}: procedure{$ENDIF}(db: psqlite3); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_complete{$IFDEF D}: function{$ENDIF}(sql: pansichar): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_complete16{$IFDEF D}: function{$ENDIF}(sql: pansichar): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

type
  busyhandler_callback = function(user: pointer; cnt: cint): cint; cdecl;

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_busy_handler{$IFDEF D}: function{$ENDIF}(db: psqlite3; cb: busyhandler_callback; user: pointer): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_busy_timeout{$IFDEF D}: function{$ENDIF}(db: psqlite3; ms: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_get_table{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;          (* An open database *)
  sql: pansichar;        (* SQL to be evaluated *)
  pResult: pppansichar;  (* Results of the query *)
  nrow: pcint;           (* Number of result rows written here *)
  ncolumn: pcint;        (* Number of result columns written here *)
  errmsg: ppansichar     (* Error msg written here *)
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_free_table{$IFDEF D}: procedure{$ENDIF}(result: ppansichar); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
//char *sqlite3_mprintf(const char*,...);
//char *sqlite3_vmprintf(const char*, va_list);
//char *sqlite3_snprintf(int,char*,const char*, ...);

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_malloc{$IFDEF D}: function{$ENDIF}(size: cint): pointer;cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_malloc64{$IFDEF D}: function{$ENDIF}(size: sqlite3_uint64): pointer;cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_realloc{$IFDEF D}: function{$ENDIF}(ptr: pointer; size: cint): pointer;cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_realloc64{$IFDEF D}: function{$ENDIF}(ptr: pointer; size: sqlite3_uint64): pointer;cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_free{$IFDEF D}: procedure{$ENDIF}(ptr: pointer);cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_msize{$IFDEF D}: function{$ENDIF}(ptr: pointer): sqlite3_uint64;cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_memory_used{$IFDEF D}: function{$ENDIF}(): sqlite3_int64; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_memory_highwater{$IFDEF D}: function{$ENDIF}(resetFlag: cint): sqlite3_int64; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_randomness{$IFDEF D}: procedure{$ENDIF}(N: cint; P: pointer); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

type
  xAuth = function(pUserData: pointer; code: cint; s1, s2, s3, s4: pansichar): cint; cdecl;

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_set_authorizer{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;
  cb: xAuth;
  pUserData: pointer
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

const
  SQLITE_DENY   = 1;   (* Abort the SQL statement with an error *)
  SQLITE_IGNORE = 2;   (* Don't allow access, but don't generate an error *)

const
  SQLITE_CREATE_INDEX         =  1;   (* Index Name      Table Name      *)
  SQLITE_CREATE_TABLE         =  2;   (* Table Name      NULL            *)
  SQLITE_CREATE_TEMP_INDEX    =  3;   (* Index Name      Table Name      *)
  SQLITE_CREATE_TEMP_TABLE    =  4;   (* Table Name      NULL            *)
  SQLITE_CREATE_TEMP_TRIGGER  =  5;   (* Trigger Name    Table Name      *)
  SQLITE_CREATE_TEMP_VIEW     =  6;   (* View Name       NULL            *)
  SQLITE_CREATE_TRIGGER       =  7;   (* Trigger Name    Table Name      *)
  SQLITE_CREATE_VIEW          =  8;   (* View Name       NULL            *)
  SQLITE_DELETE               =  9;   (* Table Name      NULL            *)
  SQLITE_DROP_INDEX           = 10;   (* Index Name      Table Name      *)
  SQLITE_DROP_TABLE           = 11;   (* Table Name      NULL            *)
  SQLITE_DROP_TEMP_INDEX      = 12;   (* Index Name      Table Name      *)
  SQLITE_DROP_TEMP_TABLE      = 13;   (* Table Name      NULL            *)
  SQLITE_DROP_TEMP_TRIGGER    = 14;   (* Trigger Name    Table Name      *)
  SQLITE_DROP_TEMP_VIEW       = 15;   (* View Name       NULL            *)
  SQLITE_DROP_TRIGGER         = 16;   (* Trigger Name    Table Name      *)
  SQLITE_DROP_VIEW            = 17;   (* View Name       NULL            *)
  SQLITE_INSERT               = 18;   (* Table Name      NULL            *)
  SQLITE_PRAGMA               = 19;   (* Pragma Name     1st arg or NULL *)
  SQLITE_READ                 = 20;   (* Table Name      Column Name     *)
  SQLITE_SELECT               = 21;   (* NULL            NULL            *)
  SQLITE_TRANSACTION          = 22;   (* NULL            NULL            *)
  SQLITE_UPDATE               = 23;   (* Table Name      Column Name     *)
  SQLITE_ATTACH               = 24;   (* Filename        NULL            *)
  SQLITE_DETACH               = 25;   (* Database Name   NULL            *)
  SQLITE_ALTER_TABLE          = 26;   (* Database Name   Table Name      *)
  SQLITE_REINDEX              = 27;   (* Index Name      NULL            *)
  SQLITE_ANALYZE              = 28;   (* Table Name      NULL            *)
  SQLITE_CREATE_VTABLE        = 29;   (* Table Name      Module Name     *)
  SQLITE_DROP_VTABLE          = 30;   (* Table Name      Module Name     *)
  SQLITE_FUNCTION             = 31;   (* Function Name   NULL            *)
  SQLITE_SAVEPOINT            = 32;   (* Operation       Savepoint Name  *)
  SQLITE_COPY                 =  0;   (* No longer used *)
  SQLITE_RECURSIVE            = 33;   (* NULL            NULL            *)


type
  xTrace = procedure(user: pointer; s: pansichar); cdecl; deprecated;
  xProfile = procedure(user: pointer; s: pansichar; i: sqlite3_uint64); cdecl; deprecated;

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_trace{$IFDEF D}: function{$ENDIF}(db: psqlite3; cb: xTrace; user: pointer): pointer; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_profile{$IFDEF D}: function{$ENDIF}(db: psqlite3; cb: xProfile; user: pointer): pointer; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

type
  progress_callback = function(user: pointer): cint; cdecl;

{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_progress_handler{$IFDEF D}: procedure{$ENDIF}(db: psqlite3; i: cint; cb: progress_callback; user: pointer); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_open{$IFDEF D}: function{$ENDIF}(
  filename: pansichar;    (* Database filename (UTF-8) *)
  ppDb: ppsqlite3         (* OUT: SQLite db handle *)
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_open16{$IFDEF D}: function{$ENDIF}(
  filename: pwidechar;    (* Database filename (UTF-16) *)
  ppDb: ppsqlite3         (* OUT: SQLite db handle *)
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_open_v2{$IFDEF D}: function{$ENDIF}(
  filename: pansichar;    (* Database filename (UTF-8) *)
  ppDb: ppsqlite3;        (* OUT: SQLite db handle *)
  flags: cint;            (* Flags *)
  zVfs: pansichar         (* Name of VFS module to use *)
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_uri_parameter{$IFDEF D}: function{$ENDIF}(zFilename : pansichar; zParam: pansichar) : pansichar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_uri_boolean{$IFDEF D}: function{$ENDIF}(zFile : pansichar; zParam :pansichar; bDefault: cint) : cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_uri_int64{$IFDEF D}: function{$ENDIF}(zFile : pansichar; zParam :pansichar; iDefault: sqlite3_int64) : sqlite3_int64; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_errcode{$IFDEF D}: function{$ENDIF}(db: psqlite3): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_extended_errcode{$IFDEF D}: function{$ENDIF}(db: psqlite3): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_errmsg{$IFDEF D}: function{$ENDIF}(db: psqlite3): pansichar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_errmsg16{$IFDEF D}: function{$ENDIF}(db: psqlite3): pwidechar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_errstr{$IFDEF D}: function{$ENDIF}(errCode: cint): pansichar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

type
  ppsqlite3_stmt = ^psqlite3_stmt;
  psqlite3_stmt = ^sqlite3_stmt;
  sqlite3_stmt = record end;


{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_limit{$IFDEF D}: function{$ENDIF}(db: psqlite3; id: cint; newVal: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}


const
  SQLITE_LIMIT_LENGTH                    = 0;
  SQLITE_LIMIT_SQL_LENGTH                = 1;
  SQLITE_LIMIT_COLUMN                    = 2;
  SQLITE_LIMIT_EXPR_DEPTH                = 3;
  SQLITE_LIMIT_COMPOUND_SELECT           = 4;
  SQLITE_LIMIT_VDBE_OP                   = 5;
  SQLITE_LIMIT_FUNCTION_ARG              = 6;
  SQLITE_LIMIT_ATTACHED                  = 7;
  SQLITE_LIMIT_LIKE_PATTERN_LENGTH       = 8;
  SQLITE_LIMIT_VARIABLE_NUMBER           = 9;
  SQLITE_LIMIT_TRIGGER_DEPTH             = 10;
  SQLITE_LIMIT_WORKER_THREADS            = 11;


{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_prepare{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;             (* Database handle *)
  zSql: pansichar;          (* SQL statement, UTF-8 encoded *)
  nByte: cint;              (* Maximum length of zSql in bytes. *)
  ppStmt: ppsqlite3_stmt;   (* OUT: Statement handle *)
  pzTail: ppansichar        (* OUT: Pointer to unused portion of zSql *)
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_prepare_v2{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;             (* Database handle *)
  zSql: pansichar;          (* SQL statement, UTF-8 encoded *)
  nByte: cint;              (* Maximum length of zSql in bytes. *)
  ppStmt: ppsqlite3_stmt;   (* OUT: Statement handle *)
  pzTail: ppansichar        (* OUT: Pointer to unused portion of zSql *)
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_prepare16{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;             (* Database handle *)
  zSql: pwidechar;          (* SQL statement, UTF-16 encoded *)
  nByte: cint;              (* Maximum length of zSql in bytes. *)
  ppStmt: ppsqlite3_stmt;   (* OUT: Statement handle *)
  pzTail: ppwidechar        (* OUT: Pointer to unused portion of zSql *)
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_prepare16_v2{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;             (* Database handle *)
  zSql: pwidechar;          (* SQL statement, UTF-16 encoded *)
  nByte: cint;              (* Maximum length of zSql in bytes. *)
  ppStmt: ppsqlite3_stmt;   (* OUT: Statement handle *)
  pzTail: ppwidechar        (* OUT: Pointer to unused portion of zSql *)
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}


{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_sql{$IFDEF D}: function{$ENDIF}(pStmt: psqlite3_stmt): pansichar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_expanded_sql{$IFDEF D}: function{$ENDIF}(pStmt: psqlite3_stmt): pansichar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}


type
  ppsqlite3_value = ^psqlite3_value;
  psqlite3_value = ^sqlite3_value;
  sqlite3_value = record end;

  psqlite3_context = ^sqlite3_context;
  sqlite3_context = record end;


{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_bind_blob{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint; V: pointer; L: cint; D: sqlite3_destructor_type): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_bind_blob64{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint; V: pointer; L: sqlite3_uint64; D: sqlite3_destructor_type): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_bind_double{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint; V: cdouble): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_bind_int{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint; V: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_bind_int64{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint; V: sqlite3_int64): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_bind_null{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_bind_text{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint; V: pansichar; L: cint; D: sqlite3_destructor_type): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_bind_text16{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint; V: pwidechar; L: cint; D: sqlite3_destructor_type): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_bind_text64{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint; V: pansichar; L: sqlite3_uint64; D: sqlite3_destructor_type; encoding: cuchar): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_bind_value{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint; V: psqlite3_value): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_bind_zeroblob{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint; V: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_bind_zeroblob64{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint; V: sqlite3_uint64): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_bind_parameter_count{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_bind_parameter_name{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint): pansichar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_bind_parameter_index{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; zName: pansichar): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_clear_bindings{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_count{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_name{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint): pansichar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_name16{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint): pwidechar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_database_name{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint): pansichar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_database_name16{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint): pwidechar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_table_name{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint): pansichar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_table_name16{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint): pwidechar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_origin_name{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint): pansichar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_origin_name16{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint): pwidechar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_decltype{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint): pansichar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_decltype16{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; N: cint): pwidechar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_step{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_data_count{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

const
  SQLITE_INTEGER  = 1;
  SQLITE_FLOAT    = 2;
  SQLITE_BLOB     = 4;
  SQLITE_NULL     = 5;
  SQLITE_TEXT     = 3;
  SQLITE3_TEXT    = 3;


{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_blob{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; iCol: cint): pointer; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_bytes{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; iCol: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_bytes16{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; iCol: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_double{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; iCol: cint): cdouble; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_int{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; iCol: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_int64{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; iCol: cint): sqlite3_int64; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_text{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; iCol: cint): pansichar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_text16{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; iCol: cint): pwidechar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_type{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; iCol: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_column_value{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; iCol: cint): psqlite3_value; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_finalize{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_reset{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

type
  xFunc = procedure(ctx: psqlite3_context; N: cint; V: ppsqlite3_value); cdecl;
  xStep = procedure(ctx: psqlite3_context; N: cint; V: ppsqlite3_value); cdecl;
  xFinal = procedure(ctx: psqlite3_context); cdecl;
  xDestroy = sqlite3_destructor_type;
  
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_create_function{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;
  zFunctionName: pansichar;
  nArg: cint;
  eTextRep: cint;
  pApp: pointer;
  funccb: xFunc;
  stepcb: xStep;
  finalcb: xFinal
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_create_function16{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;
  zFunctionName: pwidechar;
  nArg: cint;
  eTextRep: cint;
  pApp: pointer;
  funccb: xFunc;
  stepcb: xStep;
  finalcb: xFinal
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_create_function_v2{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;
  zFunctionName: pansichar;
  nArg: cint;
  eTextRep: cint;
  pApp: pointer;
  funccb: xFunc;
  stepcb: xStep;
  finalcb: xFinal;
  destroycb : xDestroy
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

const
  SQLITE_UTF8           = 1;
  SQLITE_UTF16LE        = 2;
  SQLITE_UTF16BE        = 3;
  SQLITE_UTF16          = 4;    (* Use native byte order *)
  SQLITE_ANY            = 5;    (* Deprecated *)
  SQLITE_UTF16_ALIGNED  = 8;    (* sqlite3_create_collation only *)

  SQLITE_DETERMINISTIC  = $800;

{$IFDEF SQLITE_OBSOLETE}
type
  memory_alarm_cb = function(user: pointer; i64: sqlite3_int64; i: cint): pointer; cdecl;

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_aggregate_count{$IFDEF D}: function{$ENDIF}(ctx: psqlite3_context): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_expired{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_transfer_bindings{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt; stmt2: psqlite3_stmt): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_global_recover{$IFDEF D}: function{$ENDIF}(): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_thread_cleanup{$IFDEF D}: procedure{$ENDIF}(); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_memory_alarm{$IFDEF D}: function{$ENDIF}(cb: memory_alarm_cb; user: pointer; i64: sqlite3_int64): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_value_blob{$IFDEF D}: function{$ENDIF}(val: psqlite3_value): pointer; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_value_bytes{$IFDEF D}: function{$ENDIF}(val: psqlite3_value): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_value_bytes16{$IFDEF D}: function{$ENDIF}(val: psqlite3_value): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_value_double{$IFDEF D}: function{$ENDIF}(val: psqlite3_value): cdouble; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_value_int{$IFDEF D}: function{$ENDIF}(val: psqlite3_value): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_value_int64{$IFDEF D}: function{$ENDIF}(val: psqlite3_value): sqlite3_int64; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_value_text{$IFDEF D}: function{$ENDIF}(val: psqlite3_value): pansichar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_value_text16{$IFDEF D}: function{$ENDIF}(val: psqlite3_value): pwidechar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_value_text16le{$IFDEF D}: function{$ENDIF}(val: psqlite3_value): pwidechar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_value_text16be{$IFDEF D}: function{$ENDIF}(val: psqlite3_value): pwidechar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_value_type{$IFDEF D}: function{$ENDIF}(val: psqlite3_value): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_value_numeric_type{$IFDEF D}: function{$ENDIF}(val: psqlite3_value): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_value_subtype{$IFDEF D}: function{$ENDIF}(val: psqlite3_value): cuint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_value_dup{$IFDEF D}: function{$ENDIF}(val: psqlite3_value): psqlite3_value; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_value_free{$IFDEF D}: procedure{$ENDIF}(val: psqlite3_value); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_aggregate_context{$IFDEF D}: function{$ENDIF}(ctx: psqlite3_context; nBytes: cint): pointer; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_user_data{$IFDEF D}: function{$ENDIF}(ctx: psqlite3_context): pointer; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_context_db_handle{$IFDEF D}: function{$ENDIF}(ctx: psqlite3_context): psqlite3; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}


type
  set_auxdata_cb = function(p: pointer): pointer; cdecl;

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_get_auxdata{$IFDEF D}: function{$ENDIF}(ctx: psqlite3_context; N: cint): pointer; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_set_auxdata{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context; N: cint; P: pointer; cb: set_auxdata_cb); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_blob{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context; V: pointer; N: cint; D: sqlite3_destructor_type); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_blob64{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context; V: pointer; N: sqlite3_uint64; D: sqlite3_destructor_type); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_double{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context; V: cdouble); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_error{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context; V: pansichar; N: cint); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_error16{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context; V: pwidechar; N: cint); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_error_toobig{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_error_nomem{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_error_code{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context; V: cint); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_int{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context; V: cint); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_int64{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context; V: sqlite3_int64); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_null{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_text{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context; V: pansichar; N: cint; D: sqlite3_destructor_type); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_text64{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context; V: pansichar; N: sqlite3_uint64; D: sqlite3_destructor_type; encoding: cuchar); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_text16{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context; V: pwidechar; N: cint; D: sqlite3_destructor_type); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_text16le{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context; V: pwidechar; N: cint; D: sqlite3_destructor_type); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_text16be{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context; V: pwidechar; N: cint; D: sqlite3_destructor_type); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_value{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context; V: psqlite3_value); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_result_zeroblob{$IFDEF D}: procedure{$ENDIF}(ctx: psqlite3_context; N: cint); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_result_zeroblob64{$IFDEF D}: function{$ENDIF}(ctx: psqlite3_context; N: sqlite3_uint64): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

type
  xCompare = function(user: pointer; A: cint; B: pointer; C: cint; D: pointer): cint; cdecl;

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_create_collation{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;
  zName: pansichar;
  eTextRep: cint;
  user: pointer;
  xcomparecb: xCompare
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_create_collation_v2{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;
  zName: pansichar;
  eTextRep: cint;
  user: pointer;
  xcomparecb: xCompare;
  xdestroycb: xDestroy
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_create_collation16{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;
  zName: pwidechar;
  eTextRep: cint;
  user: pointer;
  xcomparecb: xCompare
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}


type
  collation_needed_cb = function(user: pointer; db: psqlite3; eTextRep: cint; s: pansichar): pointer; cdecl;

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_collation_needed{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;
  user: pointer;
  cb: collation_needed_cb
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_collation_needed16{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;
  user: pointer;
  cb: collation_needed_cb
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_key{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;                   (* Database to be rekeyed *)
  pKey: pointer; nKey: cint       (* The key *)
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_key_v2{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;                   (* Database to be rekeyed *)
  zDbName: pansichar;             (* Name of the database *)
  pKey: pointer; nKey: cint       (* The key *)
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_rekey{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;                   (* Database to be rekeyed *)
  pKey: pointer; nKey: cint       (* The new key *)
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_rekey_v2{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;                   (* Database to be rekeyed *)
  zDbName: pansichar;             (* Name of the database *)
  pKey: pointer; nKey: cint       (* The new key *)
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_sleep{$IFDEF D}: function{$ENDIF}(M: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}


{$ifndef win32}
var
  sqlite3_temp_directory: pansichar; cvar; external {Sqlite3Lib};
  sqlite3_data_directory: pansichar; cvar; external {Sqlite3Lib};
{$endif}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_get_autocommit{$IFDEF D}: function{$ENDIF}(db: psqlite3): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_db_handle{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt): psqlite3; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_db_filename{$IFDEF D}: function{$ENDIF}(db: psqlite3; zDbName: pansichar): pansichar; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_db_readonly{$IFDEF D}: function{$ENDIF}(db: psqlite3; zDbName: pansichar): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_next_stmt{$IFDEF D}: function{$ENDIF}(db: psqlite3;stmt: psqlite3_stmt):psqlite3_stmt;cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
 
type
  commit_callback = function(user: pointer): cint; cdecl;

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_commit_hook{$IFDEF D}: function{$ENDIF}(db: psqlite3; cb: commit_callback; user: pointer): pointer; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_rollback_hook{$IFDEF D}: function{$ENDIF}(db: psqlite3; cb: sqlite3_destructor_type; user: pointer): pointer; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
type
  update_callback = procedure(user: pointer; event: cint; database, table: pansichar; rowid: sqlite3_int64); cdecl;

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_update_hook{$IFDEF D}: function{$ENDIF}(db: psqlite3; cb: update_callback; user: pointer): pointer; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_enable_shared_cache{$IFDEF D}: function{$ENDIF}(B: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_release_memory{$IFDEF D}: function{$ENDIF}(N: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_db_release_memory{$IFDEF D}: function{$ENDIF}(db: psqlite3): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_soft_heap_limit{$IFDEF D}: procedure{$ENDIF}(N: cint); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_soft_heap_limit64{$IFDEF D}: function{$ENDIF}(N: int64):int64;cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
 
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_table_column_metadata{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;             (* Connection handle *)
  zDbName: pansichar;           (* Database name or NULL *)
  zTableName: pansichar;        (* Table name *)
  zColumnName: pansichar;       (* Column name *)
  pzDataType: ppansichar;       (* OUTPUT: Declared data type *)
  pzCollSeq: ppansichar;        (* OUTPUT: Collation sequence name *)
  pNotNull: pcint;          (* OUTPUT: True if NOT NULL constracint exists *)
  pPrimaryKey: pcint;       (* OUTPUT: True if column part of PK *)
  pAutoinc: pcint           (* OUTPUT: True if column is auto-increment *)
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_load_extension{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;          (* Load the extension cinto this database connection *)
  zFile: pansichar;          (* Name of the shared library containing extension *)
  zProc: pansichar;          (* Entry point.  Derived from zFile if 0 *)
  pzErrMsg: ppansichar       (* Put error message here if not 0 *)
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_enable_load_extension{$IFDEF D}: function{$ENDIF}(db: psqlite3; onoff: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_auto_extension{$IFDEF D}: function{$ENDIF}(xEntrypoint: pointer): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_cancel_auto_extension{$IFDEF D}: function{$ENDIF}(xEntrypoint: pointer): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_reset_auto_extension{$IFDEF D}: procedure{$ENDIF}(); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}


type
  psqlite3_module = ^sqlite3_module;
  sqlite3_module = record{
    iVersion     : cint;
   // xCreate      : function(db: psqlite3; pAux: pointer; argc: cint; ): cint; cdecl;
  cint (*xCreate)(db: psqlite3; void *pAux;
               cint argc; const char *const*argv;
               sqlite3_vtab **ppVTab; char**);
  cint (*xConnect)(db: psqlite3; void *pAux;
               cint argc; const char *const*argv;
               sqlite3_vtab **ppVTab; char**);
  cint (*xBestIndex)(sqlite3_vtab *pVTab; sqlite3_index_info*);
  cint (*xDisconnect)(sqlite3_vtab *pVTab);
  cint (*xDestroy)(sqlite3_vtab *pVTab);
  cint (*xOpen)(sqlite3_vtab *pVTab; sqlite3_vtab_cursor **ppCursor);
  cint (*xClose)(sqlite3_vtab_cursor*);
  cint (*xFilter)(sqlite3_vtab_cursor*; cint idxNum; const char *idxStr;
                cint argc; sqlite3_value **argv);
    xNext         : function(pVCurs: sqlite3_vtab_cursor): cint; cdecl;
    xEof          : function(pVCurs: sqlite3_vtab_cursor): cint; cdecl;
    xColumn       : function(pVCurs: sqlite3_vtab_cursor; ctx: psqlite3_context; i: cint): cint; cdecl;
    xRowid        : function(pVCurs: sqlite3_vtab_cursor; var pRowid: sqlite3_int64): cint; cdecl;
    xUpdate       : function(pVtab: psqlite3_vtab; var v: psqlite3_value; var p: sqlite3_int64): cint; cdecl;
    xBegin        : function(pVtab: psqlite3_vtab): cint; cdecl;
    xSync         : function(pVtab: psqlite3_vtab): cint; cdecl;
    xCommit       : function(pVtab: psqlite3_vtab): cint; cdecl;
    xRollback     : function(pVtab: psqlite3_vtab): cint; cdecl;
    xFindFunction : function(pVtab: psqlite3_vtab; nArg: cint; zName: pansichar; var pxFunc: xFunc; var ppArg: pointer): cint; cdecl;
    xRename       : function(pVtab: psqlite3_vtab; zNew: pansichar): cint; cdecl;
  }end;
{.$WARNING TODO}


type
  psqlite3_index_constracint = ^sqlite3_index_constracint;
  sqlite3_index_constracint = record
     iColumn: cint;              (* Column constrained.  -1 for ROWID *)
     op: char;                   (* Constracint operator *)
     usable: char;               (* True if this constracint is usable *)
     iTermOffset: cint;          (* Used cinternally - xBestIndex should ignore *)
  end;

  psqlite3_index_orderby = ^sqlite3_index_orderby;
  sqlite3_index_orderby = record
     iColumn: cint;              (* Column number *)
     desc: char;                 (* True for DESC.  False for ASC. *)
  end;

  psqlite3_index_constracint_usage = ^sqlite3_index_constracint_usage;
  sqlite3_index_constracint_usage = record
    argvIndex: cint;             (* if >0; constracint is part of argv to xFilter *)
    omit: char;                  (* Do not code a test for this constracint *)
  end;

  psqlite3_index_info = ^sqlite3_index_info;
  sqlite3_index_info = record
    (* Inputs *)
    nConstracint: cint;         (* Number of entries in aConstracint *)
    aConstracint: psqlite3_index_constracint;
    nOrderBy: cint;             (* Number of terms in the ORDER BY clause *)
    aOrderBy: psqlite3_index_orderby;

    (* Outputs *)
    aConstracintUsage: psqlite3_index_constracint_usage;

    idxNum: cint;                 (* Number used to identify the index *)
    idxStr: pansichar;            (* String; possibly obtained from sqlite3_malloc *)
    needToFreeIdxStr: cint;       (* Free idxStr using sqlite3_free() if true *)
    orderByConsumed: cint;        (* True if output is already ordered *)
    estimatedCost: cdouble;       (* Estimated cost of using this index *)
    (* Fields below are only available in SQLite 3.8.2 and later *)
    estimatedRows: sqlite3_int64; (* Estimated number of rows returned *)
    (* Fields below are only available in SQLite 3.9.0 and later *)
    idxFlags: cint ;              (* Mask of SQLITE_INDEX_SCAN_* flags *)
    (* Fields below are only available in SQLite 3.10.0 and later *)
    colUsed: sqlite3_uint64;      (* Input: Mask of columns used by statement *)
  end;

const
  SQLITE_INDEX_SCAN_UNIQUE      = 1;
  SQLITE_INDEX_CONSTRAINT_EQ    = 2;
  SQLITE_INDEX_CONSTRAINT_GT    = 4;
  SQLITE_INDEX_CONSTRAINT_LE    = 8;
  SQLITE_INDEX_CONSTRAINT_LT    = 16;
  SQLITE_INDEX_CONSTRAINT_GE    = 32;
  SQLITE_INDEX_CONSTRAINT_MATCH = 64;
  SQLITE_INDEX_CONSTRAINT_LIKE  = 65;
  SQLITE_INDEX_CONSTRAINT_GLOB  = 66;
  SQLITE_INDEX_CONSTRAINT_REGEXP= 67;


{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_create_module{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;               (* SQLite connection to register module with *)
  zName: pansichar;           (* Name of the module *)
  module: psqlite3_module;    (* Methods for the module *)
  user: pointer               (* Client data for xCreate/xConnect *)
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_create_module_v2{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;               (* SQLite connection to register module with *)
  zName: pansichar;           (* Name of the module *)
  module: psqlite3_module;    (* Methods for the module *)
  user: pointer;              (* Client data for xCreate/xConnect *)
  xdestroycb: xDestroy        (* Module destructor function *)
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}


type
  psqlite3_vtab = ^sqlite3_vtab;
  sqlite3_vtab = record
    pModule: psqlite3_module;        (* The module for this virtual table *)
    nRef: cint;                      (* Used cinternally *)
    zErrMsg: pansichar;              (* Error message from sqlite3_mprcintf() *)
  (* Virtual table implementations will typically add additional fields *)
  end;

type
  psqlite3_vtab_cursor = ^sqlite3_vtab_cursor;
  sqlite3_vtab_cursor = record
    pVtab: psqlite3_vtab;      (* Virtual table of this cursor *)
  (* Virtual table implementations will typically add additional fields *)
  end;

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_declare_vtab{$IFDEF D}: function{$ENDIF}(db: psqlite3; zCreateTable: pansichar): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_overload_function{$IFDEF D}: function{$ENDIF}(db: psqlite3; zFuncName: pansichar; nArg: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

type
  ppsqlite3_blob = ^psqlite3_blob;
  psqlite3_blob = ^sqlite3_blob;
  sqlite3_blob = record end;


{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_blob_open{$IFDEF D}: function{$ENDIF}(
  db: psqlite3;
  zDb: pansichar;
  zTable: pansichar;
  zColumn: pansichar;
  iRow: sqlite3_int64;
  flags: cint;
  ppBlob: ppsqlite3_blob
): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}


{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_blob_reopen{$IFDEF D}: function{$ENDIF}(blob: psqlite3_blob;p:sqlite3_int64): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_blob_close{$IFDEF D}: function{$ENDIF}(blob: psqlite3_blob): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_blob_bytes{$IFDEF D}: function{$ENDIF}(blob: psqlite3_blob): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_blob_read{$IFDEF D}: function{$ENDIF}(blob: psqlite3_blob; Z: pointer; N: cint; iOffset: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_blob_write{$IFDEF D}: function{$ENDIF}(blob: psqlite3_blob; Z: pointer; N: cint; iOffset: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_vfs_find{$IFDEF D}: function{$ENDIF}(zVfsName: pansichar): psqlite3_vfs; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_vfs_register{$IFDEF D}: function{$ENDIF}(vfs: psqlite3_vfs; makeDflt: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_vfs_unregister{$IFDEF D}: function{$ENDIF}(vfs: psqlite3_vfs): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_mutex_alloc{$IFDEF D}: function{$ENDIF}(n: cint): psqlite3_mutex; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_mutex_free{$IFDEF D}: procedure{$ENDIF}(mtx: psqlite3_mutex); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_mutex_ente{$IFDEF D}: procedure{$ENDIF}(mtx: psqlite3_mutex); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_mutex_try{$IFDEF D}: function{$ENDIF}(mtx: psqlite3_mutex): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}sqlite3_mutex_leave{$IFDEF D}: procedure{$ENDIF}(mtx: psqlite3_mutex); cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_mutex_held{$IFDEF D}: function{$ENDIF}(mtx: psqlite3_mutex): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_mutex_notheld{$IFDEF D}: function{$ENDIF}(mtx: psqlite3_mutex): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_db_mutex{$IFDEF D}: function{$ENDIF}(db: psqlite3): psqlite3_mutex; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

const
  SQLITE_MUTEX_FAST             = 0;
  SQLITE_MUTEX_RECURSIVE        = 1;
  SQLITE_MUTEX_STATIC_MASTER    = 2;
  SQLITE_MUTEX_STATIC_MEM       = 3;  (* sqlite3_malloc() *)
  SQLITE_MUTEX_STATIC_MEM2      = 4;  (* sqlite3_release_memory() *)
  SQLITE_MUTEX_STATIC_PRNG      = 5;  (* sqlite3_random() *)
  SQLITE_MUTEX_STATIC_LRU       = 6;  (* lru page list *)
  SQLITE_MUTEX_STATIC_LRU2      = 7;  (* lru page list *)
  SQLITE_MUTEX_STATIC_APP1      = 8;  (* For use by application *)
  SQLITE_MUTEX_STATIC_APP2      = 9;  (* For use by application *)
  SQLITE_MUTEX_STATIC_APP3      = 10; (* For use by application *)
  SQLITE_MUTEX_STATIC_VFS1      = 11; (* For use by built-in VFS *)
  SQLITE_MUTEX_STATIC_VFS2      = 12; (* For use by extension VFS *)
  SQLITE_MUTEX_STATIC_VFS3      = 13; (* For use by application VFS *)

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_file_control{$IFDEF D}: function{$ENDIF}(db: psqlite3; zDbName: pansichar; op: cint; p: pointer): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_test_control{$IFDEF D}: function{$ENDIF}(op: cint; args: array of const): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

const
  SQLITE_TESTCTRL_FAULT_CONFIG             = 1;
  SQLITE_TESTCTRL_FAULT_FAILURES           = 2;
  SQLITE_TESTCTRL_FAULT_BENIGN_FAILURES    = 3;
  SQLITE_TESTCTRL_FAULT_PENDING            = 4;
  SQLITE_TESTCTRL_PRNG_SAVE                = 5;
  SQLITE_TESTCTRL_PRNG_RESTORE             = 6;
  SQLITE_TESTCTRL_PRNG_RESET               = 7;
  SQLITE_TESTCTRL_BITVEC_TEST              = 8;
  SQLITE_TESTCTRL_FAULT_INSTALL            = 9;
  SQLITE_TESTCTRL_BENIGN_MALLOC_HOOKS      = 10;
  SQLITE_TESTCTRL_PENDING_BYTE             = 11;
  SQLITE_TESTCTRL_ASSERT                   = 12;
  SQLITE_TESTCTRL_ALWAYS                   = 13;
  SQLITE_TESTCTRL_RESERVE                  = 14;
  SQLITE_TESTCTRL_OPTIMIZATIONS            = 15;
  SQLITE_TESTCTRL_ISKEYWORD                = 16;
  SQLITE_TESTCTRL_SCRATCHMALLOC            = 17;
  SQLITE_TESTCTRL_LOCALTIME_FAULT          = 18;
  SQLITE_TESTCTRL_EXPLAIN_STMT             = 19;  (* NOT USED *)
  SQLITE_TESTCTRL_NEVER_CORRUPT            = 20;
  SQLITE_TESTCTRL_VDBE_COVERAGE            = 21;
  SQLITE_TESTCTRL_BYTEORDER                = 22;
  SQLITE_TESTCTRL_ISINIT                   = 23;
  SQLITE_TESTCTRL_SORTER_MMAP              = 24;
  SQLITE_TESTCTRL_IMPOSTER                 = 25;
  SQLITE_TESTCTRL_LAST                     = 25;


  SQLITE_STATUS_MEMORY_USED          = 0;
  SQLITE_STATUS_PAGECACHE_USED       = 1;
  SQLITE_STATUS_PAGECACHE_OVERFLOW   = 2;
  SQLITE_STATUS_SCRATCH_USED         = 3;
  SQLITE_STATUS_SCRATCH_OVERFLOW     = 4;
  SQLITE_STATUS_MALLOC_SIZE          = 5;
  SQLITE_STATUS_PARSER_STACK         = 6;
  SQLITE_STATUS_PAGECACHE_SIZE       = 7;
  SQLITE_STATUS_SCRATCH_SIZE         = 8;
  SQLITE_STATUS_MALLOC_COUNT         = 9;

  SQLITE_DBSTATUS_LOOKASIDE_USED      = 0;
  SQLITE_DBSTATUS_CACHE_USED          = 1;
  SQLITE_DBSTATUS_SCHEMA_USED         = 2;
  SQLITE_DBSTATUS_STMT_USED           = 3;
  SQLITE_DBSTATUS_LOOKASIDE_HIT       = 4;
  SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE = 5;
  SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL = 6;
  SQLITE_DBSTATUS_CACHE_HIT           = 7;
  SQLITE_DBSTATUS_CACHE_MISS          = 8;
  SQLITE_DBSTATUS_CACHE_WRITE         = 9;
  SQLITE_DBSTATUS_DEFERRED_FKS        = 10;
  SQLITE_DBSTATUS_CACHE_USED_SHARED   = 11;
  SQLITE_DBSTATUS_MAX                 = 11;   (* Largest defined DBSTATUS *)

{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_status{$IFDEF D}: function{$ENDIF}(op: cint; pCurrent: pcint; pHighwater: pcint; resetFlag: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_status64{$IFDEF D}: function{$ENDIF}(op: cint; pCurrent: psqlite3_int64; pHighwater: psqlite3_int64; resetFlag: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_db_status{$IFDEF D}: function{$ENDIF}(db : psqlite3;op: cint; pCurrent:pcint; pHighwater: pcint; resetFlag: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF}sqlite3_stmt_status{$IFDEF D}: function{$ENDIF}(stmt: psqlite3_stmt;op: cint; pcurrent:pcint; pHighwater: pcint; resetFlag: cint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}


{Backup api}
type
  psqlite3backup = Pointer;

{$IFDEF S}function{$ELSE}var{$ENDIF} sqlite3_backup_init{$IFDEF D}: function{$ENDIF}(pDest: psqlite3; const zDestName: pansichar; pSource: psqlite3; const zSourceName: pansichar): psqlite3backup; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF} sqlite3_backup_step{$IFDEF D}: function{$ENDIF}(p: psqlite3backup; nPage: Integer): Integer; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF} sqlite3_backup_finish{$IFDEF D}: function{$ENDIF}(p: psqlite3backup): Integer; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF} sqlite3_backup_remaining{$IFDEF D}: function{$ENDIF}(p: psqlite3backup): Integer; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF} sqlite3_backup_pagecount{$IFDEF D}: function{$ENDIF}(p: psqlite3backup): Integer; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

Type
  xNotifycb = procedure (Argp: pointer; narg : cint);cdecl;
  
{$IFDEF S}function{$ELSE}var{$ENDIF} sqlite3_unlock_notify{$IFDEF D}: function{$ENDIF}(pBlocked:psqlite3;xNotify: xNotifycb;arg:pointer):cint;cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF} sqlite3_log{$IFDEF D}: procedure{$ENDIF}(iErrCode:cint;fmt : pansichar); cdecl;varargs;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

Type
  wal_hook_cb = function (p : pointer; db :psqlite3; c : pansichar; d: cint): cint;cdecl;
 
{$IFDEF S}function{$ELSE}var{$ENDIF} sqlite3_wal_hook{$IFDEF D}: function{$ENDIF}(db:psqlite3;cb : wal_hook_cb; p: pointer): pointer;cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF} sqlite3_wal_autocheckpoint{$IFDEF D}: function{$ENDIF}(db:psqlite3;n : cint): cint;cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF} 
{$IFDEF S}function{$ELSE}var{$ENDIF} sqlite3_wal_checkpoint{$IFDEF D}: function{$ENDIF}(db:psqlite3;zDB: pansichar): cint;cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF} 
{$IFDEF S}function{$ELSE}var{$ENDIF} sqlite3_wal_checkpoint_v2{$IFDEF D}: function{$ENDIF}(db:psqlite3;zDB: pansichar;emode:cint;nLog:pcint;nCkpt:pcint): cint;cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF} 

Const
  SQLITE_CHECKPOINT_PASSIVE  = 0;  (* Do as much as possible w/o blocking *)
  SQLITE_CHECKPOINT_FULL     = 1;  (* Wait for writers, then checkpoint *)
  SQLITE_CHECKPOINT_RESTART  = 2;  (* Like FULL but wait for for readers *)
  SQLITE_CHECKPOINT_TRUNCATE = 3;  (* Like RESTART but also truncate WAL *)


{String handling api}
{$IFDEF S}function{$ELSE}var{$ENDIF} sqlite3_strglob {$IFDEF D}:function{$ENDIF}(zGlob, zStr: pansichar): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}
{$IFDEF S}function{$ELSE}var{$ENDIF} sqlite3_strlike {$IFDEF D}:function{$ENDIF}(zGlob, zStr: pansichar; cEsc: cuint): cint; cdecl;{$IFDEF S}external Sqlite3Lib;{$ENDIF}

{$IFDEF LOAD_DYNAMICALLY}

function InitializeSqliteANSI(const LibraryName: AnsiString = ''): Integer; //needed as TLibraryLoadFunction
function InitializeSqlite(const LibraryName: UnicodeString = ''): Integer;
function TryInitializeSqlite(const LibraryName: Unicodestring = ''): Integer;
function ReleaseSqlite: Integer;
procedure ReleaseSqlite; //needed as TLibraryUnLoadFunction

function InitialiseSQLite: Integer; deprecated;
function InitialiseSQLite(const LibraryName: UnicodeString): Integer; deprecated;

var
  SQLiteLibraryHandle: TLibHandle;
  SQLiteDefaultLibrary: String = Sqlite3Lib;
  SQLiteLoadedLibrary: UnicodeString;
{$ENDIF LOAD_DYNAMICALLY}

implementation

function sqlite3_version(): pansichar;
begin
  Result := sqlite3_libversion();
end;

{$IFDEF LOAD_DYNAMICALLY}

resourcestring
  SErrLoadFailed     = 'Can not load SQLite client library "%s". Check your installation.';
  SErrAlreadyLoaded  = 'SQLite interface already initialized from library %s.';

procedure LoadAddresses(LibHandle: TLibHandle);
begin
  pointer(sqlite3_libversion) := GetProcedureAddress(LibHandle,'sqlite3_libversion');
  pointer(sqlite3_libversion_number) := GetProcedureAddress(LibHandle,'sqlite3_libversion_number');
  pointer(sqlite3_threadsafe) := GetProcedureAddress(LibHandle,'sqlite3_threadsafe');
  pointer(sqlite3_close) := GetProcedureAddress(LibHandle,'sqlite3_close');
  pointer(sqlite3_close_v2) := GetProcedureAddress(LibHandle,'sqlite3_close_v2');
  pointer(sqlite3_exec) := GetProcedureAddress(LibHandle,'sqlite3_exec');
  pointer(sqlite3_extended_result_codes) := GetProcedureAddress(LibHandle,'sqlite3_extended_result_codes');
  pointer(sqlite3_last_insert_rowid) := GetProcedureAddress(LibHandle,'sqlite3_last_insert_rowid');
  pointer(sqlite3_changes) := GetProcedureAddress(LibHandle,'sqlite3_changes');
  pointer(sqlite3_total_changes) := GetProcedureAddress(LibHandle,'sqlite3_total_changes');
  pointer(sqlite3_complete) := GetProcedureAddress(LibHandle,'sqlite3_complete');
  pointer(sqlite3_complete16) := GetProcedureAddress(LibHandle,'sqlite3_complete16');
  pointer(sqlite3_busy_handler) := GetProcedureAddress(LibHandle,'sqlite3_busy_handler');
  pointer(sqlite3_busy_timeout) := GetProcedureAddress(LibHandle,'sqlite3_busy_timeout');
  pointer(sqlite3_get_table) := GetProcedureAddress(LibHandle,'sqlite3_get_table');
  pointer(sqlite3_malloc) := GetProcedureAddress(LibHandle,'sqlite3_malloc');
  pointer(sqlite3_realloc) := GetProcedureAddress(LibHandle,'sqlite3_realloc');
  pointer(sqlite3_memory_used) := GetProcedureAddress(LibHandle,'sqlite3_memory_used');
  pointer(sqlite3_memory_highwater) := GetProcedureAddress(LibHandle,'sqlite3_memory_highwater');
  pointer(sqlite3_set_authorizer) := GetProcedureAddress(LibHandle,'sqlite3_set_authorizer');
  pointer(sqlite3_trace) := GetProcedureAddress(LibHandle,'sqlite3_trace');
  pointer(sqlite3_profile) := GetProcedureAddress(LibHandle,'sqlite3_profile');
  pointer(sqlite3_open) := GetProcedureAddress(LibHandle,'sqlite3_open');
  pointer(sqlite3_open16) := GetProcedureAddress(LibHandle,'sqlite3_open16');
  pointer(sqlite3_open_v2) := GetProcedureAddress(LibHandle,'sqlite3_open_v2');
  pointer(sqlite3_errcode) := GetProcedureAddress(LibHandle,'sqlite3_errcode');
  pointer(sqlite3_extended_errcode) := GetProcedureAddress(LibHandle,'sqlite3_extended_errcode');
  pointer(sqlite3_errmsg) := GetProcedureAddress(LibHandle,'sqlite3_errmsg');
  pointer(sqlite3_errmsg16) := GetProcedureAddress(LibHandle,'sqlite3_errmsg16');
  pointer(sqlite3_errstr) := GetProcedureAddress(LibHandle,'sqlite3_errstr');
  pointer(sqlite3_limit) := GetProcedureAddress(LibHandle,'sqlite3_limit');
  pointer(sqlite3_prepare) := GetProcedureAddress(LibHandle,'sqlite3_prepare');
  pointer(sqlite3_prepare_v2) := GetProcedureAddress(LibHandle,'sqlite3_prepare_v2');
  pointer(sqlite3_prepare16) := GetProcedureAddress(LibHandle,'sqlite3_prepare16');
  pointer(sqlite3_prepare16_v2) := GetProcedureAddress(LibHandle,'sqlite3_prepare16_v2');
  pointer(sqlite3_sql) := GetProcedureAddress(LibHandle,'sqlite3_sql');
  pointer(sqlite3_expanded_sql) := GetProcedureAddress(LibHandle,'sqlite3_expanded_sql');
  pointer(sqlite3_bind_blob) := GetProcedureAddress(LibHandle,'sqlite3_bind_blob');
  pointer(sqlite3_bind_blob64) := GetProcedureAddress(LibHandle,'sqlite3_bind_blob64');
  pointer(sqlite3_bind_double) := GetProcedureAddress(LibHandle,'sqlite3_bind_double');
  pointer(sqlite3_bind_int) := GetProcedureAddress(LibHandle,'sqlite3_bind_int');
  pointer(sqlite3_bind_int64) := GetProcedureAddress(LibHandle,'sqlite3_bind_int64');
  pointer(sqlite3_bind_null) := GetProcedureAddress(LibHandle,'sqlite3_bind_null');
  pointer(sqlite3_bind_text) := GetProcedureAddress(LibHandle,'sqlite3_bind_text');
  pointer(sqlite3_bind_text64) := GetProcedureAddress(LibHandle,'sqlite3_bind_text64');
  pointer(sqlite3_bind_text16) := GetProcedureAddress(LibHandle,'sqlite3_bind_text16');
  pointer(sqlite3_bind_value) := GetProcedureAddress(LibHandle,'sqlite3_bind_value');
  pointer(sqlite3_bind_zeroblob) := GetProcedureAddress(LibHandle,'sqlite3_bind_zeroblob');
  pointer(sqlite3_bind_zeroblob64) := GetProcedureAddress(LibHandle,'sqlite3_bind_zeroblob64');
  pointer(sqlite3_bind_parameter_count) := GetProcedureAddress(LibHandle,'sqlite3_bind_parameter_count');
  pointer(sqlite3_bind_parameter_name) := GetProcedureAddress(LibHandle,'sqlite3_bind_parameter_name');
  pointer(sqlite3_bind_parameter_index) := GetProcedureAddress(LibHandle,'sqlite3_bind_parameter_index');
  pointer(sqlite3_clear_bindings) := GetProcedureAddress(LibHandle,'sqlite3_clear_bindings');
  pointer(sqlite3_column_count) := GetProcedureAddress(LibHandle,'sqlite3_column_count');
  pointer(sqlite3_column_name) := GetProcedureAddress(LibHandle,'sqlite3_column_name');
  pointer(sqlite3_column_name16) := GetProcedureAddress(LibHandle,'sqlite3_column_name16');
  pointer(sqlite3_column_database_name) := GetProcedureAddress(LibHandle,'sqlite3_column_database_name');
  pointer(sqlite3_column_database_name16) := GetProcedureAddress(LibHandle,'sqlite3_column_database_name16');
  pointer(sqlite3_column_table_name) := GetProcedureAddress(LibHandle,'sqlite3_column_table_name');
  pointer(sqlite3_column_table_name16) := GetProcedureAddress(LibHandle,'sqlite3_column_table_name16');
  pointer(sqlite3_column_origin_name) := GetProcedureAddress(LibHandle,'sqlite3_column_origin_name');
  pointer(sqlite3_column_origin_name16) := GetProcedureAddress(LibHandle,'sqlite3_column_origin_name16');
  pointer(sqlite3_column_decltype) := GetProcedureAddress(LibHandle,'sqlite3_column_decltype');
  pointer(sqlite3_column_decltype16) := GetProcedureAddress(LibHandle,'sqlite3_column_decltype16');
  pointer(sqlite3_step) := GetProcedureAddress(LibHandle,'sqlite3_step');
  pointer(sqlite3_data_count) := GetProcedureAddress(LibHandle,'sqlite3_data_count');
  pointer(sqlite3_column_blob) := GetProcedureAddress(LibHandle,'sqlite3_column_blob');
  pointer(sqlite3_column_bytes) := GetProcedureAddress(LibHandle,'sqlite3_column_bytes');
  pointer(sqlite3_column_bytes16) := GetProcedureAddress(LibHandle,'sqlite3_column_bytes16');
  pointer(sqlite3_column_double) := GetProcedureAddress(LibHandle,'sqlite3_column_double');
  pointer(sqlite3_column_int) := GetProcedureAddress(LibHandle,'sqlite3_column_int');
  pointer(sqlite3_column_int64) := GetProcedureAddress(LibHandle,'sqlite3_column_int64');
  pointer(sqlite3_column_text) := GetProcedureAddress(LibHandle,'sqlite3_column_text');
  pointer(sqlite3_column_text16) := GetProcedureAddress(LibHandle,'sqlite3_column_text16');
  pointer(sqlite3_column_type) := GetProcedureAddress(LibHandle,'sqlite3_column_type');
  pointer(sqlite3_column_value) := GetProcedureAddress(LibHandle,'sqlite3_column_value');
  pointer(sqlite3_finalize) := GetProcedureAddress(LibHandle,'sqlite3_finalize');
  pointer(sqlite3_reset) := GetProcedureAddress(LibHandle,'sqlite3_reset');
  pointer(sqlite3_create_function) := GetProcedureAddress(LibHandle,'sqlite3_create_function');
  pointer(sqlite3_create_function16) := GetProcedureAddress(LibHandle,'sqlite3_create_function16');
  pointer(sqlite3_create_function_v2) := GetProcedureAddress(LibHandle,'sqlite3_create_function_v2');
  pointer(sqlite3_value_blob) := GetProcedureAddress(LibHandle,'sqlite3_value_blob');
  pointer(sqlite3_value_bytes) := GetProcedureAddress(LibHandle,'sqlite3_value_bytes');
  pointer(sqlite3_value_bytes16) := GetProcedureAddress(LibHandle,'sqlite3_value_bytes16');
  pointer(sqlite3_value_double) := GetProcedureAddress(LibHandle,'sqlite3_value_double');
  pointer(sqlite3_value_int) := GetProcedureAddress(LibHandle,'sqlite3_value_int');
  pointer(sqlite3_value_int64) := GetProcedureAddress(LibHandle,'sqlite3_value_int64');
  pointer(sqlite3_value_text) := GetProcedureAddress(LibHandle,'sqlite3_value_text');
  pointer(sqlite3_value_text16) := GetProcedureAddress(LibHandle,'sqlite3_value_text16');
  pointer(sqlite3_value_text16le) := GetProcedureAddress(LibHandle,'sqlite3_value_text16le');
  pointer(sqlite3_value_text16be) := GetProcedureAddress(LibHandle,'sqlite3_value_text16be');
  pointer(sqlite3_value_type) := GetProcedureAddress(LibHandle,'sqlite3_value_type');
  pointer(sqlite3_value_numeric_type) := GetProcedureAddress(LibHandle,'sqlite3_value_numeric_type');
  pointer(sqlite3_value_subtype) := GetProcedureAddress(LibHandle,'sqlite3_value_subtype');
  pointer(sqlite3_aggregate_context) := GetProcedureAddress(LibHandle,'sqlite3_aggregate_context');
  pointer(sqlite3_user_data) := GetProcedureAddress(LibHandle,'sqlite3_user_data');
  pointer(sqlite3_context_db_handle) := GetProcedureAddress(LibHandle,'sqlite3_context_db_handle');
  pointer(sqlite3_get_auxdata) := GetProcedureAddress(LibHandle,'sqlite3_get_auxdata');
  pointer(sqlite3_create_collation) := GetProcedureAddress(LibHandle,'sqlite3_create_collation');
  pointer(sqlite3_create_collation_v2) := GetProcedureAddress(LibHandle,'sqlite3_create_collation_v2');
  pointer(sqlite3_create_collation16) := GetProcedureAddress(LibHandle,'sqlite3_create_collation16');
  pointer(sqlite3_collation_needed) := GetProcedureAddress(LibHandle,'sqlite3_collation_needed');
  pointer(sqlite3_collation_needed16) := GetProcedureAddress(LibHandle,'sqlite3_collation_needed16');
  pointer(sqlite3_key) := GetProcedureAddress(LibHandle,'sqlite3_key');
  pointer(sqlite3_key_v2) := GetProcedureAddress(LibHandle,'sqlite3_key_v2');
  pointer(sqlite3_rekey) := GetProcedureAddress(LibHandle,'sqlite3_rekey');
  pointer(sqlite3_rekey_v2) := GetProcedureAddress(LibHandle,'sqlite3_rekey_v2');
  pointer(sqlite3_sleep) := GetProcedureAddress(LibHandle,'sqlite3_sleep');
  pointer(sqlite3_get_autocommit) := GetProcedureAddress(LibHandle,'sqlite3_get_autocommit');
  pointer(sqlite3_db_handle) := GetProcedureAddress(LibHandle,'sqlite3_db_handle');
  pointer(sqlite3_commit_hook) := GetProcedureAddress(LibHandle,'sqlite3_commit_hook');
  pointer(sqlite3_rollback_hook) := GetProcedureAddress(LibHandle,'sqlite3_rollback_hook');
  pointer(sqlite3_update_hook) := GetProcedureAddress(LibHandle,'sqlite3_update_hook');
  pointer(sqlite3_enable_shared_cache) := GetProcedureAddress(LibHandle,'sqlite3_enable_shared_cache');
  pointer(sqlite3_release_memory) := GetProcedureAddress(LibHandle,'sqlite3_release_memory');
  pointer(sqlite3_table_column_metadata) := GetProcedureAddress(LibHandle,'sqlite3_table_column_metadata');
  pointer(sqlite3_load_extension) := GetProcedureAddress(LibHandle,'sqlite3_load_extension');
  pointer(sqlite3_enable_load_extension) := GetProcedureAddress(LibHandle,'sqlite3_enable_load_extension');
  pointer(sqlite3_auto_extension) := GetProcedureAddress(LibHandle,'sqlite3_auto_extension');
  pointer(sqlite3_create_module) := GetProcedureAddress(LibHandle,'sqlite3_create_module');
  pointer(sqlite3_create_module_v2) := GetProcedureAddress(LibHandle,'sqlite3_create_module_v2');
  pointer(sqlite3_declare_vtab) := GetProcedureAddress(LibHandle,'sqlite3_declare_vtab');
  pointer(sqlite3_overload_function) := GetProcedureAddress(LibHandle,'sqlite3_overload_function');
  pointer(sqlite3_blob_open) := GetProcedureAddress(LibHandle,'sqlite3_blob_open');
  pointer(sqlite3_blob_reopen) := GetProcedureAddress(LibHandle,'sqlite3_blob_reopen');
  pointer(sqlite3_blob_close) := GetProcedureAddress(LibHandle,'sqlite3_blob_close');
  pointer(sqlite3_blob_bytes) := GetProcedureAddress(LibHandle,'sqlite3_blob_bytes');
  pointer(sqlite3_blob_read) := GetProcedureAddress(LibHandle,'sqlite3_blob_read');
  pointer(sqlite3_blob_write) := GetProcedureAddress(LibHandle,'sqlite3_blob_write');
  pointer(sqlite3_vfs_find) := GetProcedureAddress(LibHandle,'sqlite3_vfs_find');
  pointer(sqlite3_vfs_register) := GetProcedureAddress(LibHandle,'sqlite3_vfs_register');
  pointer(sqlite3_vfs_unregister) := GetProcedureAddress(LibHandle,'sqlite3_vfs_unregister');
  pointer(sqlite3_mutex_alloc) := GetProcedureAddress(LibHandle,'sqlite3_mutex_alloc');
  pointer(sqlite3_mutex_try) := GetProcedureAddress(LibHandle,'sqlite3_mutex_try');
  pointer(sqlite3_mutex_held) := GetProcedureAddress(LibHandle,'sqlite3_mutex_held');
  pointer(sqlite3_mutex_notheld) := GetProcedureAddress(LibHandle,'sqlite3_mutex_notheld');
  pointer(sqlite3_db_mutex) := GetProcedureAddress(LibHandle,'sqlite3_db_mutex');
  pointer(sqlite3_file_control) := GetProcedureAddress(LibHandle,'sqlite3_file_control');
  pointer(sqlite3_test_control) := GetProcedureAddress(LibHandle,'sqlite3_test_control');
  pointer(sqlite3_status) := GetProcedureAddress(LibHandle,'sqlite3_status');
  pointer(sqlite3_status64) := GetProcedureAddress(LibHandle,'sqlite3_status64');
  pointer(sqlite3_db_status) := GetProcedureAddress(LibHandle,'sqlite3_db_status');
  pointer(sqlite3_stmt_status) := GetProcedureAddress(LibHandle,'sqlite3_stmt_status');
  pointer(sqlite3_interrupt) := GetProcedureAddress(LibHandle,'sqlite3_interrupt');
  pointer(sqlite3_free_table) := GetProcedureAddress(LibHandle,'sqlite3_free_table');
  pointer(sqlite3_free) := GetProcedureAddress(LibHandle,'sqlite3_free');
  pointer(sqlite3_randomness) := GetProcedureAddress(LibHandle,'sqlite3_randomness');
  pointer(sqlite3_progress_handler) := GetProcedureAddress(LibHandle,'sqlite3_progress_handler');
  pointer(sqlite3_set_auxdata) := GetProcedureAddress(LibHandle,'sqlite3_set_auxdata');
  pointer(sqlite3_result_blob) := GetProcedureAddress(LibHandle,'sqlite3_result_blob');
  pointer(sqlite3_result_blob64) := GetProcedureAddress(LibHandle,'sqlite3_result_blob64');
  pointer(sqlite3_result_double) := GetProcedureAddress(LibHandle,'sqlite3_result_double');
  pointer(sqlite3_result_error) := GetProcedureAddress(LibHandle,'sqlite3_result_error');
  pointer(sqlite3_result_error16) := GetProcedureAddress(LibHandle,'sqlite3_result_error16');
  pointer(sqlite3_result_error_toobig) := GetProcedureAddress(LibHandle,'sqlite3_result_error_toobig');
  pointer(sqlite3_result_error_nomem) := GetProcedureAddress(LibHandle,'sqlite3_result_error_nomem');
  pointer(sqlite3_result_error_code) := GetProcedureAddress(LibHandle,'sqlite3_result_error_code');
  pointer(sqlite3_result_int) := GetProcedureAddress(LibHandle,'sqlite3_result_int');
  pointer(sqlite3_result_int64) := GetProcedureAddress(LibHandle,'sqlite3_result_int64');
  pointer(sqlite3_result_null) := GetProcedureAddress(LibHandle,'sqlite3_result_null');
  pointer(sqlite3_result_text) := GetProcedureAddress(LibHandle,'sqlite3_result_text');
  pointer(sqlite3_result_text64) := GetProcedureAddress(LibHandle,'sqlite3_result_text64');
  pointer(sqlite3_result_text16) := GetProcedureAddress(LibHandle,'sqlite3_result_text16');
  pointer(sqlite3_result_text16le) := GetProcedureAddress(LibHandle,'sqlite3_result_text16le');
  pointer(sqlite3_result_text16be) := GetProcedureAddress(LibHandle,'sqlite3_result_text16be');
  pointer(sqlite3_result_value) := GetProcedureAddress(LibHandle,'sqlite3_result_value');
  pointer(sqlite3_result_zeroblob) := GetProcedureAddress(LibHandle,'sqlite3_result_zeroblob');
  pointer(sqlite3_result_zeroblob64) := GetProcedureAddress(LibHandle,'sqlite3_result_zeroblob64');
  pointer(sqlite3_soft_heap_limit) := GetProcedureAddress(LibHandle,'sqlite3_soft_heap_limit');
  pointer(sqlite3_soft_heap_limit64) := GetProcedureAddress(LibHandle,'sqlite3_soft_heap_limit64');
  pointer(sqlite3_reset_auto_extension) := GetProcedureAddress(LibHandle,'sqlite3_reset_auto_extension');
  pointer(sqlite3_mutex_free) := GetProcedureAddress(LibHandle,'sqlite3_mutex_free');
  pointer(sqlite3_mutex_ente) := GetProcedureAddress(LibHandle,'sqlite3_mutex_ente');
  pointer(sqlite3_mutex_leave) := GetProcedureAddress(LibHandle,'sqlite3_mutex_leave');
  pointer(sqlite3_backup_init) := GetProcedureAddress(LibHandle,'sqlite3_backup_init');
  pointer(sqlite3_backup_step) := GetProcedureAddress(LibHandle,'sqlite3_backup_step');
  pointer(sqlite3_backup_finish) := GetProcedureAddress(LibHandle,'sqlite3_backup_finish');
  pointer(sqlite3_backup_remaining) := GetProcedureAddress(LibHandle,'sqlite3_backup_remaining');
  pointer(sqlite3_backup_pagecount) := GetProcedureAddress(LibHandle,'sqlite3_backup_pagecount');
  pointer(sqlite3_unlock_notify) := GetProcedureAddress(LibHandle,'sqlite3_unlock_notify');
  pointer(sqlite3_log) := GetProcedureAddress(LibHandle,'sqlite3_log');
  pointer(sqlite3_wal_hook) := GetProcedureAddress(LibHandle,'sqlite3_wal_hook');
  pointer(sqlite3_wal_autocheckpoint) := GetProcedureAddress(LibHandle,'sqlite3_wal_autocheckpoint');
  pointer(sqlite3_wal_checkpoint) := GetProcedureAddress(LibHandle,'sqlite3_wal_checkpoint');
  pointer(sqlite3_wal_checkpoint_v2) := GetProcedureAddress(LibHandle,'sqlite3_wal_checkpoint_v2');
  pointer(sqlite3_strlike) := GetProcedureAddress(LibHandle,'sqlite3_strlike');
   
  pointer(sqlite3_initialize) := GetProcedureAddress(LibHandle,'sqlite3_initialize');
  pointer(sqlite3_shutdown) := GetProcedureAddress(LibHandle,'sqlite3_shutdown');
  pointer(sqlite3_os_init) := GetProcedureAddress(LibHandle,'sqlite3_os_init');
  pointer(sqlite3_os_end) := GetProcedureAddress(LibHandle,'sqlite3_os_end');
  pointer(sqlite3_config) := GetProcedureAddress(LibHandle,'sqlite3_config');
  pointer(sqlite3_db_config) := GetProcedureAddress(LibHandle,'sqlite3_db_config');
  pointer(sqlite3_uri_parameter) := GetProcedureAddress(LibHandle,'sqlite3_uri_parameter');

{$IFDEF SQLITE_OBSOLETE}
  pointer(sqlite3_aggregate_count) := GetProcedureAddress(LibHandle,'sqlite3_aggregate_count');
  pointer(sqlite3_expired) := GetProcedureAddress(LibHandle,'sqlite3_expired');
  pointer(sqlite3_transfer_bindings) := GetProcedureAddress(LibHandle,'sqlite3_transfer_bindings');
  pointer(sqlite3_global_recover) := GetProcedureAddress(LibHandle,'sqlite3_global_recover');
  pointer(sqlite3_memory_alarm) := GetProcedureAddress(LibHandle,'sqlite3_memory_alarm');
  pointer(sqlite3_thread_cleanup) := GetProcedureAddress(LibHandle,'sqlite3_thread_cleanup');
{$ENDIF}
end;

var
  RefCount: Integer;

function TryInitializeSqlite(const LibraryName: UnicodeString): Integer;

Var
  N  : UnicodeString;
  
begin
  N:=LibraryName;
  if (N='') then
    N:=SQLiteDefaultLibrary;
  result:=InterlockedIncrement(RefCount);
  if result  = 1 then
  begin
    SQLiteLibraryHandle := LoadLibrary(N);
    if (SQLiteLibraryHandle = NilHandle) then
    begin
      RefCount := 0;
      Exit(-1);
    end;
    SQLiteLoadedLibrary := N;
    LoadAddresses(SQLiteLibraryHandle);
  end;
end;

function InitialiseSQLite:integer;
begin
  result:=InitializeSqlite(SQLiteDefaultLibrary);
end;

function  InitializeSQLiteANSI(const LibraryName: AnsiString):integer;
begin
  result:=InitializeSQLite(LibraryName);
end;

function  InitializeSQLite(const LibraryName: UnicodeString) :integer;
begin
  if (LibraryName<>'') and (SQLiteLoadedLibrary <> '') and (SQLiteLoadedLibrary <> LibraryName) then
    raise EInoutError.CreateFmt(SErrAlreadyLoaded,[SQLiteLoadedLibrary]);

  result:= TryInitializeSQLite(LibraryName);
  if result=-1 then
    if LibraryName='' then
      raise EInOutError.CreateFmt(SErrLoadFailed,[SQLiteDefaultLibrary])
    else
      raise EInOutError.CreateFmt(SErrLoadFailed,[LibraryName]);
end;

function  InitialiseSQLite(const LibraryName: UnicodeString):integer;
begin
  result:=InitializeSqlite(LibraryName);
end;

function  ReleaseSQLite:integer;
begin
  if InterlockedDecrement(RefCount) <= 0 then
  begin
    if SQLiteLibraryHandle <> NilHandle then
      UnloadLibrary(SQLiteLibraryHandle);
    SQLiteLibraryHandle := NilHandle;
    SQLiteLoadedLibrary := '';
    RefCount := 0;
  end;
end;

procedure ReleaseSQLite;
begin
  ReleaseSQLite;
end;

{$ENDIF}
