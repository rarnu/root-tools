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
-(void) post: (NSString *)url param: (NSString *)param;
-(void) postFile: (NSString *)url param: (NSDictionary *)param fieldName: (NSString *) fieldName fileName: (NSString *)fileName mimeType: (NSString *)mimeType file: (NSData *)file;

@end
