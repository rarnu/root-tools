#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

@interface ConfigUtils : NSObject

+(NSString *) loadConfigString: (NSString *) key;
+(int) loadConfigInt: (NSString *) key;
+(NSNumber *) loadConfigNumber: (NSString *) key;
+(BOOL) loadConfigBool: (NSString *) key;

+(void) saveConfigString: (NSString *) key value: (NSString *) val;
+(void) saveConfigInt: (NSString *) key value: (int) val;
+(void) saveConfigNumber: (NSString *) key value: (NSNumber *) val;
+(void) saveConfigBool: (NSString *) key value: (BOOL) val;

+(void)loadConfigObject: (NSString *)key obj:(id)obj;
+(void)saveConfigObject: (NSString *)key obj:(id)obj;

@end
