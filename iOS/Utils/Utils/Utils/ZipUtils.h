#import <Foundation/Foundation.h>
#import <Utils/ZipArchive.h>

@class ZipUtils;

@protocol ZipUtilsDelegate <NSObject>
@optional
-(BOOL) zipWillUnzip;
-(void) ziputils:(ZipUtils *)ziputils unzipCompleted: (BOOL)succ;
-(void) ziputils:(ZipUtils *)ziputils zipCompleted:(BOOL)succ;
@end

@interface ZipUtils : NSObject

@property id<ZipUtilsDelegate> delegate;
@property NSString * archiveFile;
@property NSString * extractPath;

-(void) unzip;
-(void) doUncompress;

-(void) unzipFile:(NSString *)file;
-(void) doUncomparessFile:(NSString *)file;

-(void) unzipCallback: (NSNumber *)ret;

-(void) zip:(NSString *)path;
-(void) zipFiles:(NSArray *)files;
-(void) doCompress:(NSString *)path;
-(void) doCompressFiles:(NSArray *)files;

-(void) zipCallback: (NSNumber *)ret;

@end

