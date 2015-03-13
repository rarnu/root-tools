#import <Foundation/Foundation.h>
#import "CommonDigest.h"

@interface MD5Utils : NSObject

+(NSString*)getFileMD5WithPath:(NSString*)path;

@end
