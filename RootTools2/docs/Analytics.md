MIUI Analytics 究竟是什么程序
===========================

* 昨天听闻 MIUI 被曝了个后门，即小米（公司）可以通过该后门安装任意程序到用户的手机。国外黑客发表了一篇文章，可以[点击查看原文](http://thehackernews.com/2016/09/xiaomi-android-backdoor.html)。这篇文章引起了很多人的共鸣，都开始声讨这个奇怪的 Analytics.apk 究竟是什么，下面我将我分析的结果予以告知。

* 首先，这是一个用于收集用户行为信息的程序，从日历，音频，视频，甚至是广告模块均可以证实，所有的小米应用都通过 Analytics.apk 上传用户的行为信息。

	![img](http://diy.ourocg.cn/mds/ana_cal_1.png)
	![img](http://diy.ourocg.cn/mds/ana_cal_2.png)
	<center>(以上代码截图来自小米内置日历)</center>
	
	![img](http://diy.ourocg.cn/mds/ana_video_1.png)
	![img](http://diy.ourocg.cn/mds/ana_video_2.png)
	<center>(以上代码截图来自小米内置视频)</center>
	
	![img](http://diy.ourocg.cn/mds/ana_ad_1.png)
	<center>(以上代码截图来自小米内置广告模块)</center>
	
* 如此频繁的调用，使得 Analytics 模块必定持续在后台运行，因为用户不管做了什么，都会间接的调起它。同时也可以解释为什么 Analytics 会耗费较多的流量，毕竟发送数据都是通过它。

* 为了更好的分析 Analytic 这个程序，我重建了整个工程，按照原文的信息进行逆向推理。

* 首先找到原文中提到的 Analytics.apk，很容易的，直接找到了相关的字样，它位于 ```com.miui.analytics.internal.l.f()```。由于 APK 经过了混淆，可能读起来并不是太方便，但是我们是可以理清逻辑的，为了方便起见，我把拥有具体含义的函数予以重命名，比如说上面的 ```f()```，重命名(利用IDE的重构工具)为``` getAnalyticsFilePath()```，以方便后续看代码时的理解。

	![img](http://diy.ourocg.cn/mds/ana_code_1.png)
	
* 那么再来看一下这个函数在什么地方被用到了，查找有两处，分别是 ```g()``` 和 ```h()``` 这两个函数。

	![img](http://diy.ourocg.cn/mds/ana_code_2.png)
	![img](http://diy.ourocg.cn/mds/ana_code_3.png)
	
* 仔细读一下就会发现，```g()``` 函数用于删除已经下载的 Analytics.apk 包文件，当包文件存在，并且已是最新版本，则删除它。而 ```h()``` 函数检查了系统内一个叫做 **IS\_CTA\_BUILD** 的变量，如果不是 **IS\_CTA\_BUILD** 并且在一段时间内未进行过检测，则检查新版本。

* 那么现在显然应该分析 ```this.aC``` 了，它是一个 ```Runnable``` 的实例，并完成检查版本的工作。我们来看一下它的代码：

	![img](http://diy.ourocg.cn/mds/ana_code_4.png)
	
* 屏幕略小无法截出全部，但是很明显的，这里在拼装一个请求，收集了设备信息并发到了 ```https://sdkconfig.ad.xiaomi.com/api/checkupdate/lastusefulversion``` 这个地址。经过 @韩道亮 老师的抓包分析，予以证实。

	![img](http://diy.ourocg.cn/mds/ana_code_5.jpeg)

* 好了，那么再来看这个请求结束后会发生什么，这里的逻辑也很清晰，服务器返回一个 JSON 字符串，然后从中解出一些关键信息，比如说 Analytics.apk 的真实下载地址之类，代码中的 ```b.a0``` 其真实内容是 ```url```，即从 JSON 中读取一个 URL 地址。

	![img](http://diy.ourocg.cn/mds/ana_code_6.png)
	
* 上述代码的最后，指出了执行另一个 Runnable 的条件，首先判断了系统内一个名为 **IS\_ALPHA\_BUILD** 的变量的值，另一个判断的条件在反编译时丢失了，smali 代码也无法读到正确的信息，只显示 ```#unknown opcode: 0x73```。综合上下文来看，有理由判断这个丢失了的 ```mVar.aF``` 变量的含义，即当前 ExecutorService 正在执行的下载任务数为 0。当满足以上两个条件时，启动另一个 Runnable 进行实际的下载工作。

* 那么代码就转到了 ```this.aE.aD```，这部分很短，也很容易理解，就是跟据之前获取到的 URL 去做下载操作，下载完成后校验 MD5 值以判断下载是否成功。需要特别注意的是，这里的 ```this.aE.ay``` 即是刚才通过 ```getAnalyticsFilePath()``` 得到的 Analytics.apk 的保存路径。

	![img](http://diy.ourocg.cn/mds/ana_code_7.png)

* 下面就是重点分析 ```this.aE.k()``` 这个方法了：

	![img](http://diy.ourocg.cn/mds/ana_code_8.png)
	
* 初看这个方法完全不知道它是做什么的，只能继续往里跟踪，很幸运的是，下一步我们就找到了我们要的终点，即安装 APK 的部分，也就是说，这段代码中的 ```new c().a(this.mContext, this.ay);``` 是完成安装工作的。而它的判断条件是 ```this.az```，这个变量由 ```this.aE.az = jSONObject.optInt("force", 0);``` 这句代码获取到，表示是否要强制安装。

	![img](http://diy.ourocg.cn/mds/ana_code_9.png)
	
* 好了，下面分析安装的代码，这段代码毫不出彩，通过反射调用了 Android 的隐藏 API，即 installPackage。可以清楚的看到，这里的调用都是按默认的操作来的，只是传入文件路径而已。

* 至此，原作者的观点得到了证实，此处的确容易被恶意利用，对于小米（公司）来说，只需要在服务器下发一个包含了强制安装标记和 APK 路径的 JSON 让更新检测程序获取到，这个 APK 就会被自动安装，其间没有经过任何的校验，如我们所想的要进行包名校验等都没有。而签名校验只是 Android 自带的校验机制，用这段代码安装任何签过名的 APK 都是可行的。

* 对于第三方黑客来说，要利用这个漏洞也很方便，拦截 ```https://sdkconfig.ad.xiaomi.com/api/checkupdate/lastusefulversion?``` 这个请求的返回数据，并把数据改成自己想要的就可以了。这样就可以实现利用这个漏洞来安装任意 APK。同样的，我顺便看了下小米的广告模块(MSA)，同样也有这个问题，只要把 APK 重命名成 App_SystemAd.apk 放到 cache 目录内，也有同样效果。

- - -

* 修复这一问题的方案如下，增加包名的检查、增加签名检查都会好很多，虽然也不是什么万全的方法，但是至少提高了成本：

	![img](http://diy.ourocg.cn/mds/ana_code_10.png)
	
* 当然了，对于大部分用户来说，或许直接禁用这个模块才是万无一失的，禁用的方法有以下几种，一种是直接禁用 APK 本身或者禁用其内部组件：

	```
	$ pm disable com.miui.analytics
	OR
	$ pm disable com.miui.analytics/com.miui.analytics.AnalyticsService
	$ pm disable com.miui.analytics/com.miui.analytics.EventService
	$ pm disable com.miui.analytics/com.miui.analytics.internal.WakeupService
	$ pm disable com.miui.analytics/com.miui.analytics.AppenderService
	$ pm disable com.miui.analytics/com.miui.analytics.AnalyticsReceiver
	$ pm disable com.miui.analytics/com.miui.analytics.internal.ApkReceiver
	```
* 也可以用一些黑科技来处理，比如说 xposed：

	```
	XposedHelpers.findAndHookMethod("com.miui.analytics.internal.l$1", loadPackageParam.classLoader, "run", XC_MethodReplacement.returnConstant(null));
	XposedHelpers.findAndHookMethod("com.miui.analytics.internal.l$2", loadPackageParam.classLoader, "run", XC_MethodReplacement.returnConstant(null));
	```
