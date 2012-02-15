package com.tencent.weibo.api;

import java.util.ArrayList;
import java.util.List;

import com.tencent.weibo.beans.OAuth;
import com.tencent.weibo.beans.QParameter;



/**
 * 其他相关API
 * 
 * @author <a href="http://blog.javawind.net">Xuefang Xu</a> 
 * @Data 2010-01-21
 * @Version 1.0.0
 */

public class Other_API extends RequestAPI {
	
	/**
	 * 我可能认识的人
	 * 
	 * @param oauth
	 * @param format
	 * @param ip
	 * @param country_code
	 * @param province_code
	 * @param city_code
	 * @return
	 * @throws Exception
	 */
	public String kownperson(OAuth oauth, String format, String ip,
			String country_code, String province_code, String city_code)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("ip", ip));
		parameters.add(new QParameter("country_code", country_code));
		parameters.add(new QParameter("province_code", province_code));
		parameters.add(new QParameter("city_code", city_code));
		return getResource("http://open.t.qq.com/api/other/kownperson",
				parameters, oauth);
	}
	
	/**
	 * 短URL变长URL
	 * @param oauth
	 * @param format
	 * @param url
	 * @return
	 * @throws Exception
	 */
	public String shorturl(OAuth oauth, String format, String url)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("url", url));
		return getResource("http://open.t.qq.com/api/other/shorturl",
				parameters, oauth);
	}
	
	/**
	 * 获取视频上传的KEY
	 * @param oauth
	 * @param format
	 * @return
	 * @throws Exception
	 */
	public String videokey(OAuth oauth, String format)
		throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		return getResource("http://open.t.qq.com/api/other/videokey",
				parameters, oauth);
	}
}
