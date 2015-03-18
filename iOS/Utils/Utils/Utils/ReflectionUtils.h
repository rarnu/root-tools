//
//  ReflectionUtils.h
//  Utils
//
//  Created by rarnu on 3/17/15.
//  Copyright (c) 2015 rarnu. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface ReflectionUtils : NSObject

+(NSMutableArray *)getClassFields: (id)obj;
+(NSMutableArray *)getClassMethods: (id)obj;
+(id)invokeClassMethod:(id)obj method:(NSString *)method param:(id)param;
+(id)invokeClassMethod:(id)obj method:(NSString *)method param1:(id)param1 param2:(id)param2;

@end
