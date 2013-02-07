# Basic Parameters  
  touch.deviceType = touchScreen  
  touch.orientationAware = 1  
  
  # Size  
  touch.size.calibration = diameter  
  touch.size.scale = 10  
  touch.size.bias = 0  
  touch.size.isSummed = 0  
  
  # Pressure  
  # Driver reports signal strength as pressure.  
  #  
  # A normal thumb touch typically registers about 200 signal strength  
  # units although we don't expect these values to be accurate.  
  touch.pressure.calibration = amplitude  
  touch.pressure.scale = 0.005  
  
  # Orientation  
  touch.orientation.calibration = none