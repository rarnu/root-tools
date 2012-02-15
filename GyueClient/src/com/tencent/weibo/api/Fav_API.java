package com.tencent.weibo.api;

import java.util.ArrayList;
import java.util.List;

import com.tencent.weibo.beans.OAuth;
import com.tencent.weibo.beans.QParameter;



/**
 * 数据收藏相关API
 * 
 * @author <a href="http://blog.javawind.net">Xuefang Xu</a> 
 * @Data 2010-01-21
 * @Version 1.0.0
 */

public class Fav_API extends RequestAPI {
 
	
	/**
	 * 收藏一条微博
	 * 
	 * @param oauth oauth标准参数
	 * @param format 返回数据的格式 是（json或xml）
	 * @param id   需要收藏的微博ID
	 * @return
	 * @throws Exception
	 */
	public String addt(OAuth oauth, String format, String id) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("id", id));
		return postContent("http://open.t.qq.com/api/fav/addt", parameters,
				oauth);
	}
	
	/**
	 * 取消收藏一条微博
	 * 
	 * @param oauth oauth标准参数
	 * @param format 返回数据的格式 是（json或xml）
	 * @param id 微博ID
	 * @return
	 * @throws Exception
	 */
	public String delt(OAuth oauth, String format, String id) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("id", id));
		return postContent("http://open.t.qq.com/api/fav/delt", parameters,
				oauth);
	}


	/**
	 * 获取收藏的微博列表
	 * 
	 * @param oauth oauth标准参数
	 * @param format 返回数据的格式 是（json或xml）
	 * @param pageflag 分页标识（0：第一页，1：向下翻页，2向上翻页）
	 * @param pagetime 向下翻页起始时间（第一页 时填0，继续翻页：填上一次请求返回的nexttime时间，）
	 * @param reqnum 每次请求记录的条数（1-20条）
	 * @param lastid 第一页 时填0,继续向下翻页，填上一次请求返回的最后一条记录ID，翻页用
	 * @return
	 * @throws Exception
	 */
	public String list_t(OAuth oauth, String format, String pageflag,
			String pagetime, String reqnum,String lastid) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("pageflag", pageflag));
		parameters.add(new QParameter("pagetime", pagetime));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("lastid", lastid));
		return getResource("http://open.t.qq.com/api/fav/list_t",
				parameters, oauth);
	}
	
	/**
	 * 订阅话题
	 * 
	 * @param oauth oauth标准参数
	 * @param format  返回数据的格式 是（json或xml）
	 * @param id 微博ID
	 * @return
	 * @throws Exception
	 */
	public String addht(OAuth oauth, String format, String id) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("id", id));
		return postContent("http://open.t.qq.com/api/fav/addht", parameters,
				oauth);
	}
	
	/**
	 * 取消收藏话题
	 * 
	 * @param oauth oauth标准参数
	 * @param format 返回数据的格式 是（json或xml）
	 * @param id 微博ID
	 * @return
	 * @throws Exception
	 */
	public String delht(OAuth oauth, String format, String id) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("id", id));
		return postContent("http://open.t.qq.com/api/fav/delht", parameters,
				oauth);
	}
 
	
	/**
	 * 获取用户已收藏的话题列表
	 * 
	 * @param oauth oauth标准参数
	 * @param format 返回数据的格式 是（json或xml）
	 * @param reqnum 请求数，最多15
	 * @param pageflag 翻页标识  0：首页   1：向下翻页 2：向上翻页
	 * @param pagetime 翻页时间戳0
	 * @param lastid 翻页话题ID，第1次请求时为0
	 * @return
	 * @throws Exception
	 */
	public String list_ht(OAuth oauth, String format, String reqnum,
			String pageflag, String pagetime,String lastid) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));		
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("pageflag", pageflag));
		parameters.add(new QParameter("pagetime", pagetime));
		parameters.add(new QParameter("lastid", lastid));
		return getResource("http://open.t.qq.com/api/fav/list_ht",
				parameters, oauth);
	}
	
	
	
}
