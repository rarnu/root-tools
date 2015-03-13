#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

@interface CacheUtils : NSObject

+(BOOL)saveCacheFile: (NSString *)filePath cacheDir: (NSString *) cacheDir;
+(NSData *)loadCacheFile: (NSString *)fileName cacheDir: (NSString *)cacheDir;
+(NSData *)loadCacheFileMD5: (NSString *)md5 cacheDir: (NSString *)cacheDir;
+(BOOL)cleanCache: (NSString *)cacheDir;

+(UIImage *)loadCacheImage: (NSString *)fileName cacheDir: (NSString *)cacheDir;
+(UIImage *)loadCacheImageMD5: (NSString *)md5 cacheDir: (NSString *)cacheDir;

@end
