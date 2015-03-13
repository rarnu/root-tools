#import <Foundation/Foundation.h>
#import "FileUtils.h"

@class DownloadUtils;

@protocol DownloadUtilsDelegate <NSObject>
@optional
-(void) downloadUtils: (DownloadUtils *)downloadUtils beginDownload: (NSInteger)beginDownload;
-(void) downloadUtils: (DownloadUtils *)downloadUtils progress: (NSInteger)progress;
-(void) downloadUtils: (DownloadUtils *)downloadUtils endDownload: (NSInteger)endDownload filePath: (NSString *)filePath;
-(void) downloadUtils: (DownloadUtils *)downloadUtils error: (NSString *)error;
@end

@interface DownloadUtils : NSObject<NSURLConnectionDataDelegate, NSURLConnectionDelegate>

@property id<DownloadUtilsDelegate> delegate;
@property NSString * fileName;
@property NSString * filePath;
@property NSInteger progress;

-(void) download: (NSString *)url name: (NSString *)name path: (NSString *)path;

@end
