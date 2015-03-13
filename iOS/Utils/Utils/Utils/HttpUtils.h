#import <Foundation/Foundation.h>

@class HttpUtils;

@protocol HttpUtilsDelegate <NSObject>
@optional
-(void)httpUtils: (HttpUtils *)httpUtils receivedData: (NSData *) data;
-(void)httpUtils: (HttpUtils *)httpUtils receivedError: (NSString *) err;
-(void)httpUtils: (HttpUtils *)httpUtils receivedFileSize: (NSInteger) fileSize;
-(void)httpUtils: (HttpUtils *)httpUtils receivedProgress: (NSInteger) progress;
@end

@interface HttpUtils : NSObject<NSURLConnectionDelegate, NSURLConnectionDataDelegate>

@property id<HttpUtilsDelegate> delegate;
@property NSMutableData * receivedData;

-(void) get: (NSString *) url;
-(void) post: (NSString *)url dict: (NSDictionary *)dict;
-(void) postFile: (NSString *)url param: (NSDictionary *)param files: (NSDictionary *)files;

+(NSArray *) getSync: (NSString *)url;
+(NSArray *) postSync: (NSString *)url dict: (NSDictionary *)dict;
+(NSArray *) postFileSync: (NSString *)url param: (NSDictionary *)param files: (NSDictionary *)files;

@end
