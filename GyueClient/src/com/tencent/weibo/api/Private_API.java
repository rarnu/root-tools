package com.tencent.weibo.api;

import java.util.ArrayList;
import java.util.List;

import com.tencent.weibo.beans.OAuth;
import com.tencent.weibo.beans.QParameter;



/**
 * 私信相关API
 * 
 * @author <a href="http://blog.javawind.net">Xuefang Xu</a> 
 * @Data 2010-01-21
 * @Version 1.0.0
 */

public class Private_API extends RequestAPI {
	/**
	 * 发一条私信
	 * 
	 * @param oauth
	 * @param format
	 * @param content
	 * @param clientip
	 * @param name
	 * @return
	 * @throws Exception
	 */
	public String add(OAuth oauth, String format, String content,
			String clientip,String name) throws Exception {
		return this.add(oauth, format, content, clientip, "", "",name);
	}

	/**
	 * 发一条私信
	 * 
	 * @param oauth
	 * @param format
	 * @param content
	 * @param clientip
	 * @param jing
	 * @param wei
	 * @param name 
	 * @return
	 * @throws Exception
	 */
	public String add(OAuth oauth, String format, String content,
			String clientip, String jing, String wei, String name) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("content", content));
		parameters.add(new QParameter("clientip", clientip));
		parameters.add(new QParameter("jing", jing));
		parameters.add(new QParameter("wei", wei));
		parameters.add(new QParameter("name", name));
		return postContent("http://open.t.qq.com/api/private/add", parameters, oauth);
	}
	
	/**
	 * 删除一条私信
	 * 
	 * @param oauth
	 * @param format
	 * @param id
	 * @return
	 * @throws Exception
	 */
	public String del(OAuth oauth, String format, String id) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("id", id));
		return postContent("http://open.t.qq.com/api/private/del", parameters, oauth);
	}
	
	/**
	 * 获取私信收件箱列表
	 * 
	 * @param oauth
	 * @param format
	 * @param pageflag
	 * @param pagetime
	 * @param reqnum
	 * @return
	 * @throws Exception
	 */
	public String recv(OAuth oauth, String format, String pageflag,
			String pagetime, String reqnum) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("pageflag", pageflag));
		parameters.add(new QParameter("pagetime", pagetime));
		parameters.add(new QParameter("reqnum", reqnum));
		return getResource("http://open.t.qq.com/api/private/recv",
				parameters, oauth);
	}
	
	/**
	 * 获取私信发件箱列表
	 * 
	 * @param oauth
	 * @param format
	 * @param pageflag
	 * @param pagetime
	 * @param reqnum
	 * @return
	 * @throws Exception
	 */
	public String send(OAuth oauth, String format, String pageflag,
			String pagetime, String reqnum) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("pageflag", pageflag));
		parameters.add(new QParameter("pagetime", pagetime));
		parameters.add(new QParameter("reqnum", reqnum));
		return getResource("http://open.t.qq.com/api/private/send",
				parameters, oauth);
	}
}
