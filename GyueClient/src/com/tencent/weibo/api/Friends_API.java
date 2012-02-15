package com.tencent.weibo.api;

import java.util.ArrayList;
import java.util.List;

import com.tencent.weibo.beans.OAuth;
import com.tencent.weibo.beans.QParameter;



/**
 * 关系链相关API
 * 
 * @author <a href="http://blog.javawind.net">Xuefang Xu</a> 
 * @Data 2010-01-21
 * @Version 1.0.0
 */

public class Friends_API extends RequestAPI {
 

	/**
	 * 我的听众列表
	 * 
	 * @param oauth 标准参数
	 * @param format 返回数据的格式 是（json或xml）
	 * @param reqnum  请求个数(1-30)
	 * @param startindex 起始位置（第一页填0，继续向下翻页：填：【reqnum*（page-1）】）
	 * @return
	 * @throws Exception
	 */
	public String fanslist(OAuth oauth, String format, String reqnum,
			String startindex) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("startindex", startindex));

		return getResource("http://open.t.qq.com/api/friends/fanslist",
				parameters, oauth);
	}

	/**
	 * 我收听的人列表
	 * 
	 * @param oauth 标准参数
	 * @param format 返回数据的格式 是（json或xml）
	 * @param reqnum  请求个数(1-30)
	 * @param startindex 起始位置（第一页填0，继续向下翻页：填：【reqnum*（page-1）】）
	 * @return
	 * @throws Exception
	 */
	public String idollist(OAuth oauth, String format, String reqnum,
			String startindex) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("startindex", startindex));

		return getResource("http://open.t.qq.com/api/friends/idollist",
				parameters, oauth);
	}

	/**
	 * 黑名单列表
	 * 
	 * @param oauth 标准参数
	 * @param format 返回数据的格式 是（json或xml）
	 * @param reqnum  请求个数(1-30)
	 * @param startindex 起始位置（第一页填0，继续向下翻页：填：【reqnum*（page-1）】）
	 * @return
	 * @throws Exception
	 */
	public String blacklist(OAuth oauth, String format, String reqnum,
			String startindex) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("startindex", startindex));

		return getResource("http://open.t.qq.com/api/friends/blacklist",
				parameters, oauth);
	}

	/**
	 * 特别收听列表
	 * 
	 * @param oauth 标准参数
	 * @param format 返回数据的格式 是（json或xml）
	 * @param reqnum  请求个数(1-30)
	 * @param startindex 起始位置（第一页填0，继续向下翻页：填：【reqnum*（page-1）】）
	 * @return
	 * @throws Exception
	 */
	// 腾讯官方API错误，此方法尚未确认
	public String speciallist(OAuth oauth, String format, String reqnum,
			String startindex) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("startindex", startindex));

		return getResource("http://open.t.qq.com/api/friends/speciallist",
				parameters, oauth);
	}

	/**
	 * 收听某个用户
	 * 
	 * @param oauth 标准参数
	 * @param format 返回数据的格式 是（json或xml）
	 * @param name 他人的帐户名列表，用","隔开
	 * @param clientip  用户浏览器IP，必须传过来，否则会当成恶意收听处理，请求被拒绝
	 * @return
	 * @throws Exception
	 */
	public String add(OAuth oauth, String format, String name,String clientip) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("name", name));
		parameters.add(new QParameter("clientip", clientip));

		return postContent("http://open.t.qq.com/api/friends/add", parameters,
				oauth);
	}

	/**
	 * 取消收听某个用户
	 * 
	 * @param oauth 标准参数
	 * @param format 回数据的格式 是（json或xml）
	 * @param name 他人的帐户名
	 * @return
	 * @throws Exception
	 */
	public String del(OAuth oauth, String format, String name) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("name", name));

		return postContent("http://open.t.qq.com/api/friends/del", parameters,
				oauth);
	}
	
	/**
	 * 特别收听某个用户
	 * 
	 * @param oauth 标准参数
	 * @param format 回数据的格式 是（json或xml）
	 * @param name 他人的帐户名
	 * @return
	 * @throws Exception
	 */
	public String addspecial(OAuth oauth, String format, String name) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("name", name));

		return postContent("http://open.t.qq.com/api/friends/addspecial", parameters,
				oauth);
	}
	
	/**
	 * 取消特别收听某个用户
	 * 
	 * @param oauth 标准参数
	 * @param format 回数据的格式 是（json或xml）
	 * @param name 他人的帐户名
	 * @return
	 * @throws Exception
	 */
	public String delspecial(OAuth oauth, String format, String name) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("name", name));

		return postContent("http://open.t.qq.com/api/friends/delspecial", parameters,
				oauth);
	}
	
	/**
	 * 添加某个用户到黑名单
	 * 
	 * @param oauth 标准参数
	 * @param format 回数据的格式 是（json或xml）
	 * @param name 他人的帐户名
	 * @return
	 * @throws Exception
	 */
	public String addblacklist(OAuth oauth, String format, String name) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("name", name));

		return postContent("http://open.t.qq.com/api/friends/addblacklist", parameters,
				oauth);
	}
	
	/**
	 * 从黑名单中删除某个用户
	 * 
	 * @param oauth 标准参数
	 * @param format 回数据的格式 是（json或xml）
	 * @param name 他人的帐户名
	 * @return
	 * @throws Exception
	 */
	public String delblacklist(OAuth oauth, String format, String name) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("name", name));

		return postContent("http://open.t.qq.com/api/friends/delblacklist", parameters,
				oauth);
	}
	
	/**
	 * 检测是否我的听众或我收听的人
	 * 
	 * @param oauth 标准参数
	 * @param format 返回数据的格式 是（json或xml）
	 * @param names 其他人的帐户名列表（最多30个）
	 * @param flag 0 检测听众，1检测收听的人 2 两种关系都检测
	 * @return
	 * @throws Exception
	 */
	public String check(OAuth oauth, String format, String names,String flag) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("names", names));
		parameters.add(new QParameter("flag", flag));

		return getResource("http://open.t.qq.com/api/friends/check", parameters,
				oauth);
	}

	/**
	 * 获取其他用户听众列表
	 * 
	 * @param oauth 标准参数
	 * @param format 返回数据的格式 是（json或xml）
	 * @param reqnum 请求个数(1-30)
	 * @param startindex 起始位置
	 * @param name 用户帐户名
	 * @return
	 * @throws Exception
	 */
	public String user_fanslist(OAuth oauth, String format, String reqnum,
			String startindex,String name) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("startindex", startindex));
		parameters.add(new QParameter("name", name));

		return getResource("http://open.t.qq.com/api/friends/user_fanslist",
				parameters, oauth);
	}
 

	/**
	 * 其他帐户收听的人列表 
	 * 
	 * @param oauth
	 * @param format
	 * @param reqnum 请求个数(1-30)
	 * @param startindex 起始位置
	 * @param name 用户帐户名
	 * @return
	 * @throws Exception
	 */
	public String user_idollist(OAuth oauth, String format, String reqnum,
			String startindex,String name) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("startindex", startindex));
		parameters.add(new QParameter("name", name));

		return getResource("http://open.t.qq.com/api/friends/user_idollist",
				parameters, oauth);
	}
	
	/**
	 * 其他帐户特别收听的人列表
	 * 
	 * @param oauth
	 * @param format
	 * @param reqnum
	 * @param startindex
	 * @param name
	 * @return
	 * @throws Exception
	 */
	public String user_speciallist(OAuth oauth, String format, String reqnum,
			String startindex,String name) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("startindex", startindex));
		parameters.add(new QParameter("name", name));

		return getResource("http://open.t.qq.com/api/friends/user_speciallist",
				parameters, oauth);
	}
	
	/**
	 * 我的粉丝列表，简单信息（200个)
	 * @param oauth
	 * @param format 返回数据的格式 是（json或xml）
	 * @param reqnum  请求个数(1-200)
	 * @param startindex 起始位置（第一页填0，继续向下翻页：填：【reqnum*（page-1）】）
	 * @return
	 * @throws Exception
	 */
	public String fanlist_s(OAuth oauth, String format, String reqnum,
			String startindex) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("startindex", startindex));
		return getResource("http://open.t.qq.com/api/friends/fanslist_s",
				parameters, oauth);
	}	



	/**
	 * 我的偶像列表，简单信息（200个)
	 * @param oauth
	 * @param format 返回数据的格式 是（json或xml）
	 * @param reqnum  请求个数(1-200)
	 * @param startindex  起始位置（第一页填0，继续向下翻页：填：【reqnum*（page-1）】）
	 * @return
	 * @throws Exception
	 */
	public String idollist_s(OAuth oauth, String format, String reqnum,
			String startindex) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("startindex", startindex));
		return getResource("http://open.t.qq.com/api/friends/idollist_s",
				parameters, oauth);
	}	
}
