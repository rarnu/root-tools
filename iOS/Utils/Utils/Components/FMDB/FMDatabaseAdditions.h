#import <Foundation/Foundation.h>
#import "FMDatabase.h"

@interface FMDatabase (FMDatabaseAdditions)


- (int)intForQuery:(NSString*)query, ...;
- (long)longForQuery:(NSString*)query, ...;
- (BOOL)boolForQuery:(NSString*)query, ...;
- (double)doubleForQuery:(NSString*)query, ...;
- (NSString*)stringForQuery:(NSString*)query, ...;
- (NSData*)dataForQuery:(NSString*)query, ...;
- (NSDate*)dateForQuery:(NSString*)query, ...;

- (BOOL)tableExists:(NSString*)tableName;
- (FMResultSet*)getSchema;
- (FMResultSet*)getTableSchema:(NSString*)tableName;
- (BOOL)columnExists:(NSString*)columnName inTableWithName:(NSString*)tableName;
- (BOOL)columnExists:(NSString*)tableName columnName:(NSString*)columnName __attribute__ ((deprecated));
- (BOOL)validateSQL:(NSString*)sql error:(NSError**)error;


#if SQLITE_VERSION_NUMBER >= 3007017
- (uint32_t)applicationID;
- (void)setApplicationID:(uint32_t)appID;
#if TARGET_OS_MAC && !TARGET_OS_IPHONE
- (NSString*)applicationIDString;
- (void)setApplicationIDString:(NSString*)string;
#endif
#endif

- (uint32_t)userVersion;
- (void)setUserVersion:(uint32_t)version;

@end
