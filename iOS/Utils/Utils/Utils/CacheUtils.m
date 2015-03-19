#import "CacheUtils.h"
#import "MD5Utils.h"

@implementation CacheUtils

static NSMutableDictionary * dictMd5;
static NSString * dictFileName = @"md5_dict";
static NSFileManager * fmgr;
static int changeDelta = 0;
static int fileCacheLimit = 100;
static NSMutableDictionary * dictMC;
static int memoryCacheLimit = 100;

+(BOOL)saveMemoryCache: (NSString *)key data:(NSData *)data {
    [self initMemoryDict];
    BOOL ret = NO;
    @try {
        if (dictMC.count < memoryCacheLimit) {
            [dictMC setValue:data forKey:key];
            ret = YES;
        }
    } @catch (NSException *exception) {
        
    }
    return ret;
}

+(NSData *)loadMemoryCache: (NSString *)key {
    [self initMemoryDict];
    NSData * ret = nil;
    @try {
        ret = [dictMC valueForKey:key];
    } @catch (NSException *exception) {
        
    }
    return ret;
}

+(void)cleanMemoryCache {
    if (dictMC) {
        [dictMC removeAllObjects];
    }
}

+(BOOL)saveCacheFile: (NSString *)filePath cacheDir: (NSString *) cacheDir {
    [self makeDir:cacheDir];
    [self initDictionary:cacheDir];
    NSString * fileMd5 = [MD5Utils getFileMD5WithPath:filePath];
    NSString * existingMd5 = dictMd5[filePath];
    BOOL ret = false;
    if (![fileMd5 isEqualToString:existingMd5]) {
        dictMd5[filePath] = fileMd5;
        NSFileManager * fmgr = [self getFileManager];
        ret = [fmgr copyItemAtPath:filePath toPath:cacheDir error:nil];
        [self saveDictionary:cacheDir];
    }
    return ret;
}

+(NSData *)loadCacheFile: (NSString *)fileName cacheDir: (NSString *)cacheDir {
    [self initDictionary:cacheDir];
    NSString * fullPath = [cacheDir stringByAppendingPathComponent:fileName];
    NSFileManager * fmgr = [self getFileManager];
    NSData * ret = nil;
    if ([fmgr fileExistsAtPath:fullPath]) {
        ret = [NSData dataWithContentsOfFile:fullPath];
    }
    return ret;
}

+(NSData *)loadCacheFileMD5: (NSString *)md5 cacheDir: (NSString *)cacheDir {
    [self initDictionary:cacheDir];
    long idx = [dictMd5.allValues indexOfObject:md5];
    NSData * ret = nil;
    if (idx != -1) {
        NSString * fullPath = [dictMd5.allKeys objectAtIndex:idx];
        ret = [NSData dataWithContentsOfFile:fullPath];
    }
    return ret;
}

+(BOOL)cleanCache: (NSString *)cacheDir {
    NSFileManager * fmgr = [self getFileManager];
    return [fmgr removeItemAtPath:cacheDir error:nil];
}

+(UIImage *)loadCacheImage: (NSString *)fileName cacheDir: (NSString *)cacheDir {
    return [UIImage imageWithData:[self loadCacheFile:fileName cacheDir:cacheDir]];
}
+(UIImage *)loadCacheImageMD5: (NSString *)md5 cacheDir: (NSString *)cacheDir {
    return [UIImage imageWithData:[self loadCacheFileMD5:md5 cacheDir:cacheDir]];
}

+(NSFileManager *)getFileManager {
    if (!fmgr) {
        fmgr = [NSFileManager defaultManager];
    }
    return fmgr;
}

+(void)makeDir: (NSString *)cacheDir {
    NSFileManager * fmgr = [self getFileManager];
    BOOL isDir;
    if (![fmgr fileExistsAtPath:cacheDir isDirectory:&isDir]) {
        if (!isDir) {
            [fmgr removeItemAtPath:cacheDir error:nil];
            [fmgr createDirectoryAtPath:cacheDir withIntermediateDirectories:YES attributes:nil error:nil];
        }
    }
}

+(void)initDictionary: (NSString *)cacheDir {
    if (!dictMd5) {
        NSString * fullPath = [cacheDir stringByAppendingPathComponent:dictFileName];
        NSFileManager * fmgr = [self getFileManager];
        if ([fmgr fileExistsAtPath:fullPath]) {
            dictMd5 = [NSMutableDictionary dictionaryWithContentsOfFile:fullPath];
        } else {
            dictMd5 = [NSMutableDictionary dictionary];
        }
    }
}

+(void)saveDictionary: (NSString *)cacheDir {
    changeDelta++;
    if (changeDelta > fileCacheLimit) {
        changeDelta = 0;
        NSString * fullPath = [cacheDir stringByAppendingPathComponent:dictFileName];
        [dictMd5 writeToFile:fullPath atomically:YES];
    }
}

+(void)initMemoryDict {
    if (!dictMC) {
        dictMC = [NSMutableDictionary dictionary];
    }
}

@end
