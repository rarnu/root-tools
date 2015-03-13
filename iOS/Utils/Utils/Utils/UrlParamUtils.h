#import <Foundation/Foundation.h>

@interface UrlParamUtils : NSObject

+(NSDictionary *)getParameters: (NSURL *) url;
+(NSString *)URLEncodedString: (NSString *)urlString;
+(NSString *)URLDecodedString: (NSString *)urlString;
@end
