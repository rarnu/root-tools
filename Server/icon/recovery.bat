@echo off
title 华为C8825D/Ascend G330C 刷Recovery专用工具 Neekh Huayi550(翻版必究)
@color 0A
:check
@echo off
@if not exist "fastboot.exe" goto nofsb
@if not exist "adb.exe" goto nofsb
@if not exist "g330c-recovery-cn.img" goto romfsb
@echo ---------------------------------------------------------------
@echo	      华为C8825D/Ascend G330C 刷Recovery专用工具
@echo.
@echo               Recovery 6.0.1.2中文版
@echo ---------------------------------------------------------------
@echo 功能介绍：
@echo     (1) 已包含Recovery 6.0.1.2中文版
@echo     (2) 整合工具和REC为一体打包！
@echo     (3) 专门为华为C8825D/Ascend G330C线刷工具！
@echo 使用步骤：
@echo     (1) 确保已正确安装了ADB驱动。
@echo.
@echo     (2) 首先进入FASTBOOT模式，音量键下和电源键。
@echo.
@echo     (3) 长按10秒左右，停在菊花不动，代表进入成功。
@echo.
@echo     (4) 插入USB数据线，连接电脑。
@echo.
@echo                            2012年9月8日 Neekh Huayi550
@echo ---------------------------------------------------------------
@echo 请确认以上步骤。然后开始刷机...
@pause >nul
goto fsb

:nofsb
@echo 缺少fastboot.exe、adb.exe等程序。
@pause
:romfsb
echo 缺少recovery映像文件，请勿随便更改recovery文件名。
@pause
@cls
goto check

:wait3
@echo ============================================================
@echo 没找到设备，请确认线缆连接。
@echo.
@echo 请确认是否进入FASTBOOT模式，是否安装ADB驱动？
@echo.
@echo 检测到似乎未安装驱动,或驱动有误.请安装驱动.
@echo.
@echo 如果安装驱动，请重新插拔USB后，启动工具.
@echo ============================================================
@echo.
@echo 输入“Y”5秒后检测，输入"N"退出。
@echo Yes.NO[Y,N]?
@set /p choice=
@if "%choice%"=="Y" goto wait4
@if "%choice%"=="y" goto wait4
@if "%choice%"=="N" exit
@if "%choice%"=="n" exit
@if "%choice%"=="Q" exit
@if "%choice%"=="q" exit
@echo 选择错误，请重试。
@echo.
@pause
@cls
@goto check

:wait4
@ping 127.0.0.1 /n 5 >nul
@goto check
@cls

:fsb
@cls
@echo ---------------------------------------------------------------
@echo	      华为C8825D/Ascend G330C 刷Recovery专用工具
@echo.
@echo               Recovery 6.0.1.2中文版
@echo ---------------------------------------------------------------
@echo 功能介绍：
@echo     (1) 已包含Recovery 6.0.1.2中文版
@echo     (2) 整合工具和REC为一体打包！
@echo     (3) 专门为华为C8825D/Ascend G330C线刷工具！
@echo 注意事项：
@echo     (1) 请勿修改Recovery的img映像名。
@echo     (2) 确保驱动安装正确，否则不能刷入。
@echo.
@echo                            2012年9月8日 by Neekh Huayi550
@echo ---------------------------------------------------------------
@echo 已进入刷机模式，准备开始刷Recovery.
@pause
@fastboot devices >nul
@echo 正在写入数据，请耐心等待！！
@echo ===============================================================
@echo 是否看见类似如下提示：                                      
@echo          "sending 'recovery' <6598>...OKAY"                
@echo          "writing 'recovery'...OKAY"                        
@echo 如果看到以上提示，证明导入成功，                            
@echo.                                                           
@echo 此时千万不要关闭此窗口                                     
@echo.                                                            
@echo 出现"< waiting for device >" 表示：驱动错误或数据线未正常链接   
@echo ===============================================================
@echo.
@echo.
@fastboot flash recovery g330c-recovery-cn.img >nul
@echo 数据写入已完成！！
@echo.
@echo 刷入REC已结束，按任意键重启手机并退出工具！！
@pause >nul
@fastboot reboot
@exit