package com.tencent.weibo.api;

import java.util.ArrayList;
import java.util.List;

import com.tencent.weibo.beans.OAuth;
import com.tencent.weibo.beans.QParameter;



/**
 * 搜索相关API
 * 
 * @author <a href="http://blog.javawind.net">Xuefang Xu</a> 
 * @Data 2010-01-21
 * @Version 1.0.0
 */

//注意：搜索相关API仅对腾讯合作方开放
public class Search_API extends RequestAPI {
	
	/**
	 * 搜索用户
	 * 
	 * @param oauth
	 * @param format
	 * @param keyword
	 * @param pagesize
	 * @param page
	 * @return
	 * @throws Exception
	 */
	public String user(OAuth oauth, String format, String keyword,
			String pagesize, String page) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("keyword", keyword));
		parameters.add(new QParameter("pagesize", pagesize));
		parameters.add(new QParameter("page", page));
		return getResource("http://open.t.qq.com/api/search/user", parameters,
				oauth);
	}

	/**
	 * 搜索广播
	 * 
	 * @param oauth
	 * @param format
	 * @param keyword
	 * @param pagesize 每页大小
	 * @param page 页码
	 * @return
	 * @throws Exception
	 */
	public String t(OAuth oauth, String format, String keyword,
			String pagesize, String page) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("keyword", keyword));
		parameters.add(new QParameter("pagesize", pagesize));
		parameters.add(new QParameter("page", page));
		return getResource("http://open.t.qq.com/api/search/t", parameters,
				oauth);
	}

	/**
	 * 搜索用户通过标签（合作者权限）
	 * 
	 * @param oauth
	 * @param format
	 * @param keyword
	 * @param pagesize
	 * @param page
	 * @return
	 * @throws Exception
	 */
	public String userbytag(OAuth oauth, String format, String keyword,
			String pagesize, String page) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("keyword", keyword));
		parameters.add(new QParameter("pagesize", pagesize));
		parameters.add(new QParameter("page", page));
		return getResource("http://open.t.qq.com/api/search/userbytag", parameters,
				oauth);
	}
}
