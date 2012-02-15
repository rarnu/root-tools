package com.tencent.weibo.api;

import java.util.ArrayList;
import java.util.List;

import com.tencent.weibo.beans.OAuth;
import com.tencent.weibo.beans.QParameter;



/**
 * 话题相关API
 * 
 * @author <a href="http://blog.javawind.net">Xuefang Xu</a> 
 * @Data 2010-01-21
 * @Version 1.0.0
 */

public class Ht_API extends RequestAPI {

	/**
	 * 根据话题名称查话题ID
	 * 
	 * @param oauth
	 * @param format
	 * @param httexts
	 * @return
	 * @throws Exception
	 */
	public String ids(OAuth oauth, String format, String httexts)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("httexts", httexts));
		return getResource("http://open.t.qq.com/api/ht/ids", parameters, oauth);
	}

	/**
	 * 根据话题ID获取话题相关信息
	 * 
	 * @param oauth
	 * @param format
	 * @param ids
	 * @return
	 * @throws Exception
	 */
	public String info(OAuth oauth, String format, String ids)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("ids", ids));
		return getResource("http://open.t.qq.com/api/ht/info", parameters, oauth);
	}
}
