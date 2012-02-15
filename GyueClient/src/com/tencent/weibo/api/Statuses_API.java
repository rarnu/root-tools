package com.tencent.weibo.api;

import java.util.ArrayList;
import java.util.List;

import com.tencent.weibo.beans.OAuth;
import com.tencent.weibo.beans.QParameter;



/**
 * 时间线相关API
 * 
 * @author <a href="http://blog.javawind.net">Xuefang Xu</a> 
 * @Data 2010-01-21
 * @Version 1.0.0
 */

public class Statuses_API extends RequestAPI {

	/**
	 * 主页时间线
	 * 
	 * @param oauth
	 * @param format
	 * @param pageflag
	 * @param pagetime
	 * @param reqnum
	 * @return
	 * @throws Exception
	 */
	public String home_timeline(OAuth oauth, String format, String pageflag,
			String pagetime, String reqnum) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("pageflag", pageflag));
		parameters.add(new QParameter("pagetime", pagetime));
		parameters.add(new QParameter("reqnum", reqnum));
		return getResource("http://open.t.qq.com/api/statuses/home_timeline",
				parameters, oauth);
	}

	/**
	 * 广播大厅时间线
	 * 
	 * @param oauth
	 * @param format
	 * @param pos
	 * @param reqnum
	 * @return
	 * @throws Exception
	 */
	public String public_timeline(OAuth oauth, String format, String pos,
			String reqnum) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("pos", pos));
		parameters.add(new QParameter("reqnum", reqnum));
		return getResource("http://open.t.qq.com/api/statuses/public_timeline",
				parameters, oauth);
	}

	/**
	 * 其他用户发表时间线
	 * 
	 * @param oauth
	 * @param format
	 * @param pageflag
	 * @param pagetime
	 * @param reqnum
	 * @param name
	 * @return
	 * @throws Exception
	 */
	public String user_timeline(OAuth oauth, String format, String pageflag,
			String pagetime, String reqnum, String name) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("pageflag", pageflag));
		parameters.add(new QParameter("pagetime", pagetime));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("name", name));
		return getResource("http://open.t.qq.com/api/statuses/user_timeline",
				parameters, oauth);
	}

	/**
	 * 用户提及时间线
	 * 
	 * @param oauth
	 * @param format
	 * @param pageflag
	 * @param pagetime
	 * @param reqnum
	 * @param lastid
	 * @return
	 * @throws Exception
	 */
	public String mentions_timeline(OAuth oauth, String format,
			String pageflag, String pagetime, String reqnum, String lastid)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("pageflag", pageflag));
		parameters.add(new QParameter("pagetime", pagetime));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("lastid", lastid));
		return getResource(
				"http://open.t.qq.com/api/statuses/mentions_timeline",
				parameters, oauth);
	}

	/**
	 * 话题时间线
	 * 
	 * @param oauth
	 * @param format
	 * @param httext
	 * @param pageflag
	 * @param pageinfo
	 * @param reqnum
	 * @return
	 * @throws Exception
	 */
	public String ht_timeline(OAuth oauth, String format, String httext,
			String pageflag, String pageinfo, String reqnum) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("httext", httext));
		parameters.add(new QParameter("pageflag", pageflag));
		parameters.add(new QParameter("pageinfo", pageinfo));
		parameters.add(new QParameter("reqnum", reqnum));
		return getResource("http://open.t.qq.com/api/statuses/ht_timeline",
				parameters, oauth);
	}



	/**
	 * 我发表时间线
	 * @param oauth
	 * @param format  返回数据的格式 是（json或xml）
	 * @param pageflag 分页标识（0：第一页，1：向下翻页，2向上翻页）
	 * @param pagetime 本页起始时间（第一页 时填0，继续向下翻页：填上一次请求返回的最后一条记录时间）
	 * @param reqnum  每次请求记录的条数（1-100条）
	 * @param lastid 第一页 时填0,继续向下翻页，填上一次请求返回的最后一条记录ID，翻页用
	 * @param type 拉取类型, 0x1 原创发表 0x2 转载 0x8 回复 0x10 空回 0x20 提及 0x40 点评 如需拉取多个类型请|上(0x1|0x2) 得到3，type=3即可,填零表示拉取所有类型
	 * @param contenttype 内容过滤 填零表示所有类型 1-带文本 2-带链接 4图片 8-带视频 0x10-带音频
	 * @param accesslevel 权限标识 1 表示只显示我发表的
	 * @return
	 * @throws Exception
	 */
	public String broadcast_timeline(OAuth oauth, String format,
			String pageflag, String pagetime, String reqnum, String lastid,
			String type,String contenttype,String accesslevel)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("pageflag", pageflag));
		parameters.add(new QParameter("pagetime", pagetime));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("lastid", lastid));
		parameters.add(new QParameter("type", type));
		parameters.add(new QParameter("contenttype", contenttype));
		parameters.add(new QParameter("accesslevel", accesslevel));
		//parameters.add(new QParameter("name", name));
		return getResource(
				"http://open.t.qq.com/api/statuses/broadcast_timeline",
				parameters, oauth);
	}
	
	/**
	 * 特别收听的人发表时间线
	 * @param oauth
	 * @param format
	 * @param pageflag
	 * @param pagetime
	 * @param reqnum
	 * @return
	 * @throws Exception
	 */
	public String special_timeline(OAuth oauth, String format,
			String pageflag, String pagetime, String reqnum)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("pageflag", pageflag));
		parameters.add(new QParameter("pagetime", pagetime));
		parameters.add(new QParameter("reqnum", reqnum));
		return getResource(
				"http://open.t.qq.com/api/statuses/special_timeline",
				parameters, oauth);
	}
	/**
	 * 地区发表时间线 
	 * @param oauth
	 * @param format
	 * @param pageflag
	 * @param pagetime
	 * @param reqnum
	 * @return
	 * @throws Exception
	 */
	public String area_timeline(OAuth oauth, String format,
			String pos,String reqnum,String country,String province,
			String city)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("pos", pos));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("country", country));
		parameters.add(new QParameter("province", province));
		parameters.add(new QParameter("city", city));
		
		return getResource(
				"http://open.t.qq.com/api/statuses/area_timeline",
				parameters, oauth);
	}		
	

	
	/**
	 * 主页时间线索引
	 * @param oauth
	 * @param format 返回数据的格式 是（json或xml）
	 * @param pageflag 分页标识（0：第一页，1：向下翻页，2向上翻页）
	 * @param pagetime  本页起始时间（第一页 时填0，继续翻页：填上一次请求返回的最后一条记录时间）
	 * @param reqnum  每次请求记录的条数（1-300条）
	 * @param type 拉取类型, 0x1 原创发表 0x2 转载 0x8 回复 0x10 空回 0x20 提及 0x40 点评 如需拉取多个类型请|上(0x1|0x2) 得到3，type=3即可,填零表示拉取所有类型
	 * @param contenttype 内容过滤 填零表示所有类型 1-带文本 2-带链接 4图片 8-带视频 0x10-带音频
	 * @param accesslevel 权限标识 1 表示只显示我发表的
	 * @return
	 * @throws Exception
	 */
	public String home_timeline_ids(OAuth oauth, String format,
			String pageflag ,String pagetime,String reqnum,String type,
			String contenttype,String accesslevel)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("pageflag", pageflag));
		parameters.add(new QParameter("pagetime", pagetime));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("type", type));
		parameters.add(new QParameter("contenttype", contenttype));
		parameters.add(new QParameter("accesslevel", accesslevel));
		
		return getResource(
				"http://open.t.qq.com/api/statuses/home_timeline_ids",
				parameters, oauth);
	}
	

	
	/**
	 * 其他用户发表时间线索引
	 * @param oauth
	 * @param format 返回数据的格式 是（json或xml）
	 * @param pageflag 分页标识（0：第一页，1：向下翻页，2向上翻页）
	 * @param pagetime 本页起始时间（第第一页 时填0，继续翻页：填上一次请求返回的最后一条记录时间，
	 * @param reqnum 每次请求记录的条数（1-300条）
	 * @param lastid 第一页 时填0,继续向下翻页，填上一次请求返回的最后一条记录ID，翻页用
	 * @param name 你需要读取该用户的用户名
	 * @param type  拉取类型, 0x1 原创发表 0x2 转载 0x8 回复 0x10 空回 0x20 提及 0x40 点评 	如需拉取多个类型请|上(0x1|0x2) 得到3，type=3即可,填零表示拉取所有类型
	 * @param contenttype 内容过滤 填零表示所有类型 1-带文本 2-带链接 4图片 8-带视频 0x10-带音频
	 * @param accesslevel 权限标识 1 表示只显示我发表的默认填零
	 * @return
	 * @throws Exception
	 */
	public String user_timeline_ids(OAuth oauth, String format,
			String pageflag ,String pagetime,String reqnum,String lastid,
			String name,String type,String contenttype,String accesslevel)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("pageflag", pageflag));
		parameters.add(new QParameter("pagetime", pagetime));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("lastid", lastid));
		parameters.add(new QParameter("name", name));
		parameters.add(new QParameter("type", type));
		parameters.add(new QParameter("contenttype", contenttype));
		parameters.add(new QParameter("accesslevel", accesslevel));
		
		return getResource(
				"http://open.t.qq.com/api/statuses/user_timeline_ids",
				parameters, oauth);
	}	

	/**
	 * 我发表时间线索引
	 * @param oauth
	 * @param format 返回数据的格式 是（json或xml）
	 * @param pageflag 分页标识（0：第一页，1：向下翻页，2向上翻页）
	 * @param pagetime 本页起始时间（第一页 时填0，继续向下翻页：填上一次请求返回的最后一条记录时间）
	 * @param reqnum 每次请求记录的条数（1-300条）
	 * @param lastid 第一页 时填0,继续向下翻页，填上一次请求返回的最后一条记录ID，翻页用
	 * @param type 拉取类型, 0x1 原创发表 0x2 转载 0x8 回复 0x10 空回 0x20 提及 0x40 点评 如需拉取多个类型请|上(0x1|0x2) 得到3，type=3即可,填零表示拉取所有类型
	 * @param contenttype 内容过滤 填零表示所有类型 1-带文本 2-带链接 4图片 8-带视频 0x10-带音频
	 * @param accesslevel 权限标识 1 表示只显示我发表的
	 * @return
	 * @throws Exception
	 */
	public String broadcast_timeline_ids(OAuth oauth, String format,
			String pageflag ,String pagetime,String reqnum,String lastid,
			String type,String contenttype,String accesslevel)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("pageflag", pageflag));
		parameters.add(new QParameter("pagetime", pagetime));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("lastid", lastid));
		parameters.add(new QParameter("type", type));
		parameters.add(new QParameter("contenttype", contenttype));
		parameters.add(new QParameter("accesslevel", accesslevel));
		
		return getResource(
				"http://open.t.qq.com/api/statuses/broadcast_timeline_ids",
				parameters, oauth);
	}	
	
	/**
	 * 用户提及时间线索引
	 * @param oauth 返回数据的格式 是（json或xml）
	 * @param format 分页标识（0：第一页，1：向下翻页，2向上翻页）
	 * @param pagetime 本页起始时间（第一页 时填0，继续向下翻页：填上一次请求返回的最后一条记录时间）
	 * @param reqnum 每次请求记录的条数（1-300条）
	 * @param lastid 第一页 时填0,继续向下翻页，填上一次请求返回的最后一条记录ID，翻页用
	 * @param type 拉取类型, 0x1 原创发表 0x2 转载 0x8 回复 0x10 空回 0x20 提及 0x40 点评 	如需拉取多个类型请|上(0x1|0x2) 得到3，type=3即可,填零表示拉取所有类型
	 * @param contenttype 内容过滤 填零表示所有类型 1-带文本 2-带链接 4图片 8-带视频 0x10-带音频
	 * @param accesslevel 权限标识 1 表示只显示好友提及我的，0x2 表示名人提及我的
	 * @return
	 * @throws Exception
	 */
	public String mentions_timeline_ids(OAuth oauth, String format,
			String pagetime,String reqnum,String lastid,
			String type,String contenttype,String accesslevel)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("pagetime", pagetime));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("lastid", lastid));
		parameters.add(new QParameter("type", type));
		parameters.add(new QParameter("contenttype", contenttype));
		parameters.add(new QParameter("accesslevel", accesslevel));
		
		return getResource(
				"http://open.t.qq.com/api/statuses/mentions_timeline_ids",
				parameters, oauth);
	}	
	

	/**
	 * 多用户发表时间线
	 * @param oauth
	 * @param format 返回数据的格式 是（json或xml）
	 * @param pageflag 分页标识（0：第一页，1：向下翻页，2向上翻页）
	 * @param pagetime 本页起始时间（第第一页 时填0，继续翻页：填上一次请求返回的最后一条记录时间，
	 * @param reqnum 每次请求记录的条数（1-100条）
	 * @param lastid 第一页 时填0,继续向下翻页，填上一次请求返回的最后一条记录ID，翻页用
	 * @param names 你需要读取用户列表用“,”隔开，例如：abc,bcde,effg
	 * @param type 拉取类型, 0x1 原创发表 0x2 转载 0x8 回复 0x10 空回 0x20 提及 0x40 点评
	 * @param contenttype 内容过滤 填零表示所有类型 1-带文本 2-带链接 4图片 8-带视频 0x10-带音频
	 * @param accesslevel 权限标识 1 表示只显示我发表的默认填零
	 * @return
	 * @throws Exception
	 */
	public String users_timeline(OAuth oauth, String format,String pageflag,
			String pagetime,String reqnum,String lastid,
			String names,String type,String contenttype,String accesslevel)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("pagetime", pagetime));
		parameters.add(new QParameter("pageflag", pageflag));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("lastid", lastid));
		parameters.add(new QParameter("names", names));
		parameters.add(new QParameter("type", type));
		parameters.add(new QParameter("contenttype", contenttype));
		parameters.add(new QParameter("accesslevel", accesslevel));
		
		return getResource(
				"http://open.t.qq.com/api/statuses/users_timeline",
				parameters, oauth);
	}

	
	/**
	 *  多用户发表时间线索引
	 * @param oauth
	 * @param format 返回数据的格式 是（json或xml）
	 * @param pageflag 分页标识（0：第一页，1：向下翻页，2向上翻页）
	 * @param pagetime 本页起始时间（第第一页 时填0，继续翻页：填上一次请求返回的最后一条记录时间，
	 * @param reqnum 每次请求记录的条数（1-100条）
	 * @param lastid 第一页 时填0,继续向下翻页，填上一次请求返回的最后一条记录ID，翻页用
	 * @param names 你需要读取用户列表用“,”隔开，例如：abc,bcde,effg
	 * @param type 拉取类型, 0x1 原创发表 0x2 转载 0x8 回复 0x10 空回 0x20 提及 0x40 点评
	 * @param contenttype 内容过滤 填零表示所有类型 1-带文本 2-带链接 4图片 8-带视频 0x10-带音频
	 * @param accesslevel 权限标识 1 表示只显示我发表的默认填零
	 * @return
	 * @throws Exception
	 */
	public String users_timeline_ids(OAuth oauth, String format,String pageflag,
			String pagetime,String reqnum,String lastid,
			String names,String type,String contenttype,String accesslevel)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("pagetime", pagetime));
		parameters.add(new QParameter("pageflag", pageflag));
		parameters.add(new QParameter("reqnum", reqnum));
		parameters.add(new QParameter("lastid", lastid));
		parameters.add(new QParameter("names", names));
		parameters.add(new QParameter("type", type));
		parameters.add(new QParameter("contenttype", contenttype));
		parameters.add(new QParameter("accesslevel", accesslevel));
		
		return getResource(
				"http://open.t.qq.com/api/statuses/users_timeline_ids",
				parameters, oauth);
	}
}
