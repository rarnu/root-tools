#import <Foundation/Foundation.h>
#import "sqlite3.h"

@class FMDatabase;

@interface FMDatabasePool : NSObject {
    NSString            *_path;
    dispatch_queue_t    _lockQueue;
    NSMutableArray      *_databaseInPool;
    NSMutableArray      *_databaseOutPool;
    __unsafe_unretained id _delegate;
    NSUInteger          _maximumNumberOfDatabasesToCreate;
    int                 _openFlags;
}

@property (atomic, retain) NSString *path;
@property (atomic, assign) id delegate;
@property (atomic, assign) NSUInteger maximumNumberOfDatabasesToCreate;
@property (atomic, readonly) int openFlags;

+ (instancetype)databasePoolWithPath:(NSString*)aPath;
+ (instancetype)databasePoolWithPath:(NSString*)aPath flags:(int)openFlags;
- (instancetype)initWithPath:(NSString*)aPath;
- (instancetype)initWithPath:(NSString*)aPath flags:(int)openFlags;
- (NSUInteger)countOfCheckedInDatabases;
- (NSUInteger)countOfCheckedOutDatabases;
- (NSUInteger)countOfOpenDatabases;
- (void)releaseAllDatabases;
- (void)inDatabase:(void (^)(FMDatabase *db))block;
- (void)inTransaction:(void (^)(FMDatabase *db, BOOL *rollback))block;
- (void)inDeferredTransaction:(void (^)(FMDatabase *db, BOOL *rollback))block;

#if SQLITE_VERSION_NUMBER >= 3007000
- (NSError*)inSavePoint:(void (^)(FMDatabase *db, BOOL *rollback))block;
#endif

@end

@interface NSObject (FMDatabasePoolDelegate)

- (BOOL)databasePool:(FMDatabasePool*)pool shouldAddDatabaseToPool:(FMDatabase*)database;
- (void)databasePool:(FMDatabasePool*)pool didAddDatabase:(FMDatabase*)database;

@end

