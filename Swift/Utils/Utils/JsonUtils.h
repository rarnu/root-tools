#import <Foundation/Foundation.h>

@interface JsonUtils : NSObject

+(NSMutableArray *)getClassFields: (id)obj;
+(void)fillJsonToObject:(id)obj jsonString:(NSString *)jsonString;
+(void)fillJsonToObject:(id)obj jsonData:(NSData *)jsonData;
+(void)fillJsonToObject:(id)obj json:(NSJSONSerialization *)json;
+(NSString *)objectToJsonString: (id)obj;

@end
