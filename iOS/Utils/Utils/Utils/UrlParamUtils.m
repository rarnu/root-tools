#import "UrlParamUtils.h"

@implementation UrlParamUtils

+(NSDictionary *)getParameters: (NSURL *) url {
    NSMutableString * host = [NSMutableString stringWithString:[url description]];
    [host replaceOccurrencesOfString:@"yibansc://" withString:@"" options:NSCaseInsensitiveSearch range:NSMakeRange(0, host.length)];
    NSArray * hArr = [host componentsSeparatedByString:@"&"];
    NSMutableDictionary * dict = [NSMutableDictionary dictionary];
    for (NSString * s in hArr) {
        if (![s isEqualToString:@""]) {
            NSArray * sarr = [s componentsSeparatedByString:@"="];
            dict[sarr[0]] = [self URLDecodedString:sarr[1]];
        }
    }
    return dict;
}

+ (NSString *)URLEncodedString: (NSString *)urlString {
    return [urlString stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
}

+(NSString *)URLDecodedString: (NSString *)urlString {
    return [urlString stringByReplacingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
}

@end

