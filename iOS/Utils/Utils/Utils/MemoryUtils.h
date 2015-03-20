#import <Foundation/Foundation.h>

@interface MemoryInfo : NSObject
@property double total;
@property double wired;
@property double active;
@property double inactive;
@property double free;
@property double resident;
@end

@interface MemoryUtils : NSObject

+(MemoryInfo *)getMemoryInfo;

@end
