#import <Foundation/Foundation.h>
#import "sqlite3.h"

@interface CursorUtils : NSObject

+(NSArray *)getColumns: (sqlite3_stmt *)stmt;
+(void)fillCursorToObject: (sqlite3_stmt *)stmt object: (id)obj;

// types: field types
// i: int
// s: string
// d: double
// b: blob
+(void)fillSqliteStmt: (sqlite3_stmt *)stmt object: (id)obj types: (NSString *)types;
+(NSString *)buildSplittedString: (NSArray *)array splitter: (NSString *)splitter;

@end
