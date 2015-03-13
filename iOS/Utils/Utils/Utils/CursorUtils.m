#import "CursorUtils.h"

@implementation CursorUtils

+(NSArray *)getColumns: (sqlite3_stmt *)stmt {
    NSMutableArray * arr = [NSMutableArray array];
    int count = sqlite3_column_count(stmt);
    for (int i=0; i<count; i++) {
        [arr addObject:[NSString stringWithUTF8String:(char *)sqlite3_column_name(stmt, i)]];
    }
    return arr;
}

+(void)fillCursorToObject: (sqlite3_stmt *)stmt object: (id)obj {
    NSArray * arrColumns = [CursorUtils getColumns:stmt];
    NSArray * arrFields = [JsonUtils getClassFields:obj];
    NSMutableString * dbKey;
    int idx;
    int dbType;
    
    for (NSString * key in arrFields) {
        dbKey = [NSMutableString stringWithString:key];
        if ([dbKey hasPrefix:@"_"]) {
            [dbKey deleteCharactersInRange:NSMakeRange(0, 1)];
        }
        idx = (int)[arrColumns indexOfObject:dbKey];
        dbType = sqlite3_column_type(stmt, idx);
        switch (dbType) {
            case SQLITE_TEXT:
                [obj setValue:[NSString stringWithUTF8String:(char *)sqlite3_column_text(stmt, idx)] forKey:key];
                break;
            case SQLITE_FLOAT:
                [obj setValue:[NSNumber numberWithDouble:sqlite3_column_double(stmt, idx)] forKey:key];
                break;
            case SQLITE_INTEGER:
                [obj setValue:[NSNumber numberWithInt:sqlite3_column_int(stmt, idx)] forKey:key];
                break;
            case SQLITE_BLOB:
                [obj setValue:(__bridge id)sqlite3_column_blob(stmt, idx) forKey:key];
                break;
            default:
                break;
        }
    }
}

// types: field types
// i: int
// s: string
// d: double
// b: blob
+(void)fillSqliteStmt: (sqlite3_stmt *)stmt object: (id)obj types: (NSString *)types; {
    NSArray * arrFields = [JsonUtils getClassFields:obj];
    NSString * key;
    NSString * dbType;
    for (int i=0; i<types.length; i++) {
        key = [arrFields objectAtIndex:i];
        dbType = [types substringWithRange:NSMakeRange(i, 1)];
        if ([dbType isEqualToString:@"i"]) {
            sqlite3_bind_int(stmt, i+1, [[obj valueForKey:key] intValue]);
        } else if ([dbType isEqualToString:@"s"]) {
            sqlite3_bind_text(stmt, i+1, [[obj valueForKey:key] UTF8String], -1, nil);
        } else if ([dbType isEqualToString:@"d"]) {
            sqlite3_bind_double(stmt, i+1, [[obj valueForKey:key] doubleValue]);
        } else if ([dbType isEqualToString:@"b"]) {
            sqlite3_bind_blob(stmt, i+1, (__bridge void *)[obj valueForKey:key], -1, nil);
        }
    }
}

+(NSString *)buildSplittedString: (NSArray *)array splitter: (NSString *)splitter {
    NSMutableString * str = [NSMutableString stringWithString:@""];
    for (NSString * s in array) {
        [str appendFormat:@"%@%@", s, splitter];
    }
    if ([str hasSuffix:splitter]) {
        [str deleteCharactersInRange:NSMakeRange(str.length - 1, 1)];
    }
    return str;
}

@end
