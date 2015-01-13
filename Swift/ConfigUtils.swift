import UIKit

let KEY_BACKGROUND = "background"

class ConfigUtils: NSObject {
    
    class func loadConfigString(key: String) -> String? {
        return loadConfigObj(key) as? String
    }
    
    class func loadConfigInt(key: String) -> Int? {
        return loadConfigObj(key) as? Int
    }
    
    class func loadConfigDouble(key: String) -> Double? {
        return loadConfigObj(key) as? Double
    }
    
    class func loadConfigBool(key: String) -> Bool? {
        return loadConfigObj(key) as? Bool
    }
    
    class func loadConfigObj(key: String) -> AnyObject? {
        return NSUserDefaults.standardUserDefaults().objectForKey(key)
    }
    
    class func saveConfigString(key: String, val: String) {
        saveConfigObj(key, val: val)
    }
    
    class func saveConfigInt(key: String, val: Int) {
        saveConfigObj(key, val: val)
    }
    
    class func saveConfigDouble(key: String, val: Double) {
        saveConfigObj(key, val: val)
    }
    
    class func saveConfigBool(key: String, val: Bool) {
        saveConfigObj(key, val: val)
    }
    
    class func saveConfigObj(key: String, val: AnyObject) {
        var def = NSUserDefaults.standardUserDefaults()
        def.setObject(val, forKey: key)
        def.synchronize()
    }
}
