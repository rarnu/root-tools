import UIKit

class DeviceInfo: NSObject {
    var deviceName: String?
    var uuid: String?
    var system: String?
    var systemVersion: String?
    var model: String?
    var appName: String?
    var appVersion: String?
    var appBuild: String?
}

class DeviceUtils: NSObject {
    
    class func getDevieInfo() -> DeviceInfo {
        var device = UIDevice()
        var dicApp = NSBundle.mainBundle().infoDictionary
        var info = DeviceInfo()
        info.deviceName = device.name
        info.uuid = device.identifierForVendor.UUIDString
        info.system = device.systemName
        info.systemVersion = device.systemVersion
        info.model = device.model
        info.appName = dicApp?["CFBundleDisplayName"] as? String
        info.appVersion = dicApp?["CFBundleShortVersionString"] as? String
        info.appBuild = dicApp?["CFBundleVersion"] as? String
        return info
    }
    
    
}
