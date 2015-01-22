#import <Foundation/Foundation.h>
#import <Utils/ZipArchive.h>

@class ZipUtils;

@protocol ZipUtilsDelegate <NSObject>
@optional
-(BOOL) zipWillUnzip;
-(void) ziputils: (ZipUtils *)ziputils unzipCompleted: (BOOL)succ;
@end

@interface ZipUtils : NSObject

@property id<ZipUtilsDelegate> delegate;
@property NSString * archiveFile;
@property NSString * extractPath;

-(void) unzip;
-(void) doUncompress;
-(void) callback: (NSNumber *)ret;

@end

