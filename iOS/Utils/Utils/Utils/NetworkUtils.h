//
//  NetworkUtils.h
//  Utils
//
//  Created by rarnu on 3/17/15.
//  Copyright (c) 2015 rarnu. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>
#import <SystemConfiguration/SystemConfiguration.h>
#import <netinet/in.h>

#define NT_NONE 0
#define NT_WIFI 1
#define NT_MOBILE 2

@interface NetworkUtils : NSObject

+(BOOL) isConnected;
+(int) getNetworkType;

extern NSString *kReachabilityChangedNotification;

+ (instancetype)reachabilityWithHostName:(NSString *)hostName;
+ (instancetype)reachabilityWithAddress:(const struct sockaddr_in *)hostAddress;
+ (instancetype)reachabilityForInternetConnection;
+ (instancetype)reachabilityForLocalWiFi;
- (BOOL)startNotifier;
- (void)stopNotifier;
- (int)currentReachabilityStatus;
- (BOOL)connectionRequired;

@end
