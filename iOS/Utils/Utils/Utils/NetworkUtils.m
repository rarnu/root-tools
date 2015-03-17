#import "NetworkUtils.h"
#import <arpa/inet.h>
#import <ifaddrs.h>
#import <netdb.h>
#import <sys/socket.h>
#import <CoreFoundation/CoreFoundation.h>

#define HOST_NAME @"www.baidu.com"

NSString *kReachabilityChangedNotification = @"kNetworkReachabilityChangedNotification";

static void ReachabilityCallback(SCNetworkReachabilityRef target, SCNetworkReachabilityFlags flags, void* info) {
    NetworkUtils* noteObject = (__bridge NetworkUtils *)info;
    [[NSNotificationCenter defaultCenter] postNotificationName: kReachabilityChangedNotification object: noteObject];
}

@implementation NetworkUtils {
    BOOL _alwaysReturnLocalWiFiStatus;
    SCNetworkReachabilityRef _reachabilityRef;
}

+(BOOL) isConnected {
    NetworkUtils * r = [NetworkUtils reachabilityWithHostName:HOST_NAME];
    return [r currentReachabilityStatus] != NT_NONE;
}

+(int) getNetworkType {
    NetworkUtils * r = [NetworkUtils reachabilityWithHostName:HOST_NAME];
    return [r currentReachabilityStatus];
}

+ (instancetype)reachabilityWithHostName:(NSString *)hostName {
    NetworkUtils* returnValue = NULL;
    SCNetworkReachabilityRef reachability = SCNetworkReachabilityCreateWithName(NULL, [hostName UTF8String]);
    if (reachability != NULL) {
        returnValue= [[self alloc] init];
        if (returnValue != NULL) {
            returnValue->_reachabilityRef = reachability;
            returnValue->_alwaysReturnLocalWiFiStatus = NO;
        }
    }
    return returnValue;
}

+ (instancetype)reachabilityWithAddress:(const struct sockaddr_in *)hostAddress {
    SCNetworkReachabilityRef reachability = SCNetworkReachabilityCreateWithAddress(kCFAllocatorDefault, (const struct sockaddr *)hostAddress);
    
    NetworkUtils* returnValue = NULL;
    if (reachability != NULL) {
        returnValue = [[self alloc] init];
        if (returnValue != NULL) {
            returnValue->_reachabilityRef = reachability;
            returnValue->_alwaysReturnLocalWiFiStatus = NO;
        }
    }
    return returnValue;
}

+ (instancetype)reachabilityForInternetConnection {
    struct sockaddr_in zeroAddress;
    bzero(&zeroAddress, sizeof(zeroAddress));
    zeroAddress.sin_len = sizeof(zeroAddress);
    zeroAddress.sin_family = AF_INET;
    return [self reachabilityWithAddress:&zeroAddress];
}


+ (instancetype)reachabilityForLocalWiFi {
    struct sockaddr_in localWifiAddress;
    bzero(&localWifiAddress, sizeof(localWifiAddress));
    localWifiAddress.sin_len = sizeof(localWifiAddress);
    localWifiAddress.sin_family = AF_INET;
    
    localWifiAddress.sin_addr.s_addr = htonl(IN_LINKLOCALNETNUM);
    
    NetworkUtils* returnValue = [self reachabilityWithAddress: &localWifiAddress];
    if (returnValue != NULL) {
        returnValue->_alwaysReturnLocalWiFiStatus = YES;
    }
    
    return returnValue;
}

- (BOOL)startNotifier {
    BOOL returnValue = NO;
    SCNetworkReachabilityContext context = {0, (__bridge void *)(self), NULL, NULL, NULL};
    
    if (SCNetworkReachabilitySetCallback(_reachabilityRef, ReachabilityCallback, &context)) {
        if (SCNetworkReachabilityScheduleWithRunLoop(_reachabilityRef, CFRunLoopGetCurrent(), kCFRunLoopDefaultMode)) {
            returnValue = YES;
        }
    }
    
    return returnValue;
}


- (void)stopNotifier {
    if (_reachabilityRef != NULL) {
        SCNetworkReachabilityUnscheduleFromRunLoop(_reachabilityRef, CFRunLoopGetCurrent(), kCFRunLoopDefaultMode);
    }
}


- (void)dealloc {
    [self stopNotifier];
    if (_reachabilityRef != NULL) {
        CFRelease(_reachabilityRef);
    }
}

- (int)localWiFiStatusForFlags:(SCNetworkReachabilityFlags)flags {
    int returnValue = NT_NONE;
    if ((flags & kSCNetworkReachabilityFlagsReachable) && (flags & kSCNetworkReachabilityFlagsIsDirect)) {
        returnValue = NT_WIFI;
    }
    return returnValue;
}


- (int)networkStatusForFlags:(SCNetworkReachabilityFlags)flags {
    if ((flags & kSCNetworkReachabilityFlagsReachable) == 0) {
        return NT_NONE;
    }
    
    int returnValue = NT_NONE;
    if ((flags & kSCNetworkReachabilityFlagsConnectionRequired) == 0) {
        returnValue = NT_WIFI;
    }
    
    if ((((flags & kSCNetworkReachabilityFlagsConnectionOnDemand ) != 0) ||
         (flags & kSCNetworkReachabilityFlagsConnectionOnTraffic) != 0)) {
        
        if ((flags & kSCNetworkReachabilityFlagsInterventionRequired) == 0) {
            returnValue = NT_WIFI;
        }
    }
    
    if ((flags & kSCNetworkReachabilityFlagsIsWWAN) == kSCNetworkReachabilityFlagsIsWWAN) {
        returnValue = NT_MOBILE;
    }
    
    return returnValue;
}


- (BOOL)connectionRequired {
    SCNetworkReachabilityFlags flags;
    if (SCNetworkReachabilityGetFlags(_reachabilityRef, &flags)) {
        return (flags & kSCNetworkReachabilityFlagsConnectionRequired);
    }
    
    return NO;
}


- (int)currentReachabilityStatus {
    int returnValue = NT_NONE;
    SCNetworkReachabilityFlags flags;
    if (SCNetworkReachabilityGetFlags(_reachabilityRef, &flags)) {
        if (_alwaysReturnLocalWiFiStatus) {
            returnValue = [self localWiFiStatusForFlags:flags];
        } else {
            returnValue = [self networkStatusForFlags:flags];
        }
    }
    
    return returnValue;
}

@end
