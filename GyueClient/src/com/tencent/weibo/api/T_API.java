package com.tencent.weibo.api;

import java.util.ArrayList;
import java.util.List;
 

import com.tencent.weibo.beans.OAuth;
import com.tencent.weibo.beans.QParameter;
 



/**
 * 微博相关API
 * 
 * @author <a href="http://blog.javawind.net">Xuefang Xu</a> 
 * @Data 2010-01-21
 * @Version 1.0.0
 */

public class T_API extends RequestAPI {
 
	/**
	 * 获取一条微博数据
	 * 
	 * @param oauth
	 * @param format
	 * @param id
	 * @return
	 * @throws Exception
	 */
	public String show(OAuth oauth, String format, String id) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("id", id));
		return getResource("http://open.t.qq.com/api/t/show", parameters, oauth);
	}

	/**
	 * 发表一条微博
	 * 
	 * @param oauth
	 * @param format
	 * @param content
	 * @param clientip
	 * @return
	 * @throws Exception
	 */
	public String add(OAuth oauth, String format, String content,
			String clientip) throws Exception {
		return this.add(oauth, format, content, clientip, "", "");
	}

	/**
	 * 发表一条微博
	 * 
	 * @param oauth
	 * @param format
	 * @param content
	 * @param clientip
	 * @param jing
	 * @param wei
	 * @return
	 * @throws Exception
	 */
	public String add(OAuth oauth, String format, String content,
			String clientip, String jing, String wei) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("content", content));
		parameters.add(new QParameter("clientip", clientip));
		parameters.add(new QParameter("jing", jing));
		parameters.add(new QParameter("wei", wei));
		return postContent("http://open.t.qq.com/api/t/add", parameters, oauth);
	}

	/**
	 * 删除一条微博数据
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
		return postContent("http://open.t.qq.com/api/t/del", parameters, oauth);
	}

	/**
	 * 转播一条微博
	 * 
	 * @param oauth
	 * @param format
	 * @param content
	 * @param clientip
	 * @param reid
	 * @return
	 * @throws Exception
	 */
	public String re_add(OAuth oauth, String format, String content,
			String clientip, String reid) throws Exception {
		return this.re_add(oauth, format, content, clientip, "", "", reid);
	}

	/**
	 * 转播一条微博
	 * 
	 * @param oauth
	 * @param format
	 * @param content
	 * @param clientip
	 * @param jing
	 * @param wei
	 * @param reid
	 * @return
	 * @throws Exception
	 */
	public String re_add(OAuth oauth, String format, String content,
			String clientip, String jing, String wei, String reid)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("content", content));
		parameters.add(new QParameter("clientip", clientip));
		parameters.add(new QParameter("jing", jing));
		parameters.add(new QParameter("wei", wei));
		parameters.add(new QParameter("reid", reid));
		return postContent("http://open.t.qq.com/api/t/re_add", parameters, oauth);
	}

	/**
	 * 点评一条微博
	 * 
	 * @param oauth
	 * @param format
	 * @param content
	 * @param clientip
	 * @param reid
	 * @return
	 * @throws Exception
	 */
	public String comment(OAuth oauth, String format, String content,
			String clientip, String reid) throws Exception {
 

		return this.comment(oauth, format, content, clientip, "", "", reid);
	}

	/**
	 * 点评一条微博
	 * 
	 * @param oauth
	 * @param format
	 * @param content
	 * @param clientip
	 * @param jing
	 * @param wei
	 * @param reid
	 * @return
	 * @throws Exception
	 */
	public String comment(OAuth oauth, String format, String content,
			String clientip, String jing, String wei, String reid)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("content", content));
		parameters.add(new QParameter("clientip", clientip));
		parameters.add(new QParameter("jing", jing));
		parameters.add(new QParameter("wei", wei));
		parameters.add(new QParameter("reid", reid));
		return postContent("http://open.t.qq.com/api/t/comment", parameters,
				oauth);
	}
	
	/**
	 * 回复一条微博
	 * 
	 * @param oauth
	 * @param format
	 * @param content
	 * @param clientip
	 * @param reid
	 * @return
	 * @throws Exception
	 */
	public String reply(OAuth oauth, String format, String content,
			String clientip, String reid) throws Exception {
		return this.reply(oauth, format, content, clientip, "", "", reid);
	}

	/**
	 * 回复一条微博
	 * 
	 * @param oauth
	 * @param format
	 * @param content
	 * @param clientip
	 * @param jing
	 * @param wei
	 * @param reid
	 * @return
	 * @throws Exception
	 */
	public String reply(OAuth oauth, String format, String content,
			String clientip, String jing, String wei, String reid)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("content", content));
		parameters.add(new QParameter("clientip", clientip));
		parameters.add(new QParameter("jing", jing));
		parameters.add(new QParameter("wei", wei));
		parameters.add(new QParameter("reid", reid));
		return postContent("http://open.t.qq.com/api/t/reply", parameters,
				oauth);
	}

	/**
	 * 发表一条带图片的微博
	 * 
	 * @param oauth
	 * @param format
	 * @param content
	 * @param clientip
	 * @param picpath
	 * @return
	 * @throws Exception
	 */
	public String add_pic(OAuth oauth, String format, String content,
			String clientip, String picpath) throws Exception {
		return this.add_pic(oauth, format, content, clientip, "", "", picpath);
	}

	/**
	 * 发表一条带图片的微博
	 * 
	 * @param oauth
	 * @param format
	 * @param content
	 * @param clientip
	 * @param jing
	 * @param wei
	 * @param picpath
	 * @return
	 * @throws Exception
	 */
	public String add_pic(OAuth oauth, String format, String content,
			String clientip, String jing, String wei, String picpath)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("content", content));
		parameters.add(new QParameter("clientip", clientip));
		parameters.add(new QParameter("jing", jing));
		parameters.add(new QParameter("wei", wei));
		
		List<QParameter> pic = new ArrayList<QParameter>();
		pic.add(new QParameter("pic", picpath));
		return postFile("http://open.t.qq.com/api/t/add_pic", parameters, pic,
				oauth);
	}

	/**
	 * 获取微博当前已被转播次数
	 * 
	 * @param oauth
	 * @param format
	 * @param ids
	 * @return
	 * @throws Exception
	 */
	public String re_count(OAuth oauth, String format, String ids)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("ids", ids));
		return getResource("http://open.t.qq.com/api/t/re_count", parameters,
				oauth);
	}

	/**
	 * 获取单条微博的转播理由/点评列表
	 * 
	 * @param oauth
	 * @param format
	 * @param rootid
	 * @param pageflag
	 * @param pagetime
	 * @return
	 * @throws Exception
	 */
	public String re_list(OAuth oauth, String format, String rootid,
			String pageflag, String pagetime) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("rootid", rootid));
		parameters.add(new QParameter("pageflag", pageflag));
		parameters.add(new QParameter("pagetime", pagetime));
		return getResource("http://open.t.qq.com/api/t/re_list", parameters,
				oauth);
	}
 

	
	/**
	 * 发表音乐微博 
	 * @param oauth
	 * @param format 返回数据的格式 是（json或xml）
	 * @param content  微博内容
	 * @param clientip 用户IP（必填）用户浏览器IP,
	 * @param jing 经度（可以填空）
	 * @param wei 纬度（可以填空）
	 * @param url 音乐地址
	 * @param title 音乐名
	 * @param author 演唱者
	 * @return
	 * @throws Exception
	 */
	public String add_music(
			OAuth oauth, String format, String content,
			String clientip, String jing, String wei,String url,String title,
			String author) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("content", content));
		parameters.add(new QParameter("clientip", clientip));
		parameters.add(new QParameter("jing", jing));
		parameters.add(new QParameter("wei", wei));
		parameters.add(new QParameter("url", url));
		parameters.add(new QParameter("title", title));
		parameters.add(new QParameter("author", author));
		
		return postContent("http://open.t.qq.com/api/t/add_music", parameters,
				oauth);
	}

	/**
	 * 发表视频微博
	 * @param oauth
	 * @param format 返回数据的格式 是（json或xml）
	 * @param content  微博内容
	 * @param clientip 用户IP（必填）用户浏览器IP,
	 * @param jing  经度（可以填空）
	 * @param wei 纬度（可以填空）
	 * @param url 视频地址，后台自动分析视频信息，支持youku,tudou,ku6
	 * @return
	 * @throws Exception
	 */
	public String add_video(
			OAuth oauth, String format, String content,
			String clientip, String jing, String wei,String url
			) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("content", content));
		parameters.add(new QParameter("clientip", clientip));
		parameters.add(new QParameter("jing", jing));
		parameters.add(new QParameter("wei", wei));
		parameters.add(new QParameter("url", url));
		return postContent("http://open.t.qq.com/api/t/add_video", parameters,
				oauth);
	}
	
	
	/**
	 * 获取视频信息
	 * @param oauth
	 * @param format 返回数据的格式 是（json或xml）
	 * @param url 视频地址，后台自动分析视频信息，支持youku,tudou,ku6
	 * @return
	 * @throws Exception
	 */
	public String getvideoinfo(
			OAuth oauth, String format,String url
			) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("url", url));
		return postContent("http://open.t.qq.com/api/t/getvideoinfo", parameters,
				oauth);
	}
	
	
	
	/**
	 * 根据微博ID批量获取微博内容（与索引合起来用）
	 * @param oauth
	 * @param format  数据返回的格式（json或XML）
	 * @param ids 微博ID列表，用“,”隔开
	 * @return
	 * @throws Exception
	 */
	public String list(
			OAuth oauth, String format,String ids
			) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("ids", ids));
		return getResource("http://open.t.qq.com/api/t/list", parameters,
				oauth);
	}
	

	/**
	 * 预发表一条视频微博
	 * @param oauth
	 * @param format 返回数据的格式 是（json或xml）
	 * @param content  微博内容
	 * @param clientip 用户IP(以分析用户所在地)
	 * @param jing 经度（可以填空）
	 * @param wei 纬度（可以填空）
	 * @param vid 视频ID
	 * @return
	 * @throws Exception
	 */
	/*
	public String add_video_prev(
			OAuth oauth, String format, String content,
			String clientip, String jing, String wei,String vid
		) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("content", content));
		parameters.add(new QParameter("clientip", clientip));
		parameters.add(new QParameter("jing", jing));
		parameters.add(new QParameter("wei", wei));
		parameters.add(new QParameter("vid", vid));
		 
		return postContent("http://open.t.qq.com/api/t/add_video_prev", parameters,
				oauth);
	}
	*/	
}
