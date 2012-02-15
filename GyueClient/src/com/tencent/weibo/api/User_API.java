package com.tencent.weibo.api;

import java.util.ArrayList;
import java.util.List;

import com.tencent.weibo.beans.OAuth;
import com.tencent.weibo.beans.QParameter;



/**
 * 帐户相关API
 * 
 * @author <a href="http://blog.javawind.net">Xuefang Xu</a> 
 * @Data 2010-01-21
 * @Version 1.0.0
 */

public class User_API extends RequestAPI {

	/**
	 * 获取自己的资料
	 * 
	 * @param oauth
	 * @param format
	 * @return
	 * @throws Exception
	 */
	public String info(OAuth oauth, String format) throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		return getResource("http://open.t.qq.com/api/user/info", parameters,
				oauth);
	}

	/**
	 * 修改/更新用户本人信息
	 * 
	 * @param oauth
	 * @param format
	 * @param nick
	 * @param sex
	 * @param year
	 * @param month
	 * @param day
	 * @param countrycode
	 * @param provincecode
	 * @param citycode
	 * @param introduction
	 * @return
	 * @throws Exception
	 */
	public String update(OAuth oauth, String format, String nick, String sex,
			String year, String month, String day, String countrycode,
			String provincecode, String citycode, String introduction)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("nick", nick));
		parameters.add(new QParameter("sex", sex));
		parameters.add(new QParameter("year", year));
		parameters.add(new QParameter("month", month));
		parameters.add(new QParameter("day", day));
		parameters.add(new QParameter("countrycode", countrycode));
		parameters.add(new QParameter("provincecode", provincecode));
		parameters.add(new QParameter("citycode", citycode));
		parameters.add(new QParameter("introduction", introduction));
		return postContent("http://open.t.qq.com/api/user/update", parameters,
				oauth);
	}

	/**
	 * 修改/更新用户本人头像
	 * 
	 * @param oauth
	 * @param format
	 * @param picpath
	 * @return
	 * @throws Exception
	 */
	public String update_head(OAuth oauth, String format, String picpath)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		
		List<QParameter> pic = new ArrayList<QParameter>();
		pic.add(new QParameter("pic", picpath));
		return postFile("http://open.t.qq.com/api/user/update_head", parameters,
				pic, oauth);
	}

	/**
	 * 获取其他用户个人资料
	 * 
	 * @param oauth
	 * @param format
	 * @param name
	 * @return
	 * @throws Exception
	 */
	public String other_info(OAuth oauth, String format, String name)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("name", name));
		return getResource("http://open.t.qq.com/api/user/other_info",
				parameters, oauth);
	}



	/**
	 * 
	 * @param oauth
	 * @param format 返回数据的格式 是（json或xml）
	 * @param feildid 教育信息记录ID （添加feildid=1.修改填返回的ID,删除下面四个参数为空）
	 * @param year  入学年限
	 * @param Schoolid 学校ID
	 * @param departmentid 院系ID
	 * @param level 学历
	 * @return
	 * @throws Exception
	 */
	public String update_edu(OAuth oauth, String format, String feildid,String year,
			String schoolid,String departmentid,String level
			)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("feildid", feildid));
		parameters.add(new QParameter("year", year));
		parameters.add(new QParameter("schoolid", schoolid));
		parameters.add(new QParameter("departmentid", departmentid));
		parameters.add(new QParameter("level", level));
 
		return postContent("http://open.t.qq.com/api/user/update_edu",
				parameters, oauth);
	}	
	
	/**
	 * 获取一批人的简单资料
	 * @param oauth
	 * @param format 返回数据的格式 是（json或xml）
	 * @param names 用户ID列表 比如 abc,edf,xxxx
	 * @return
	 * @throws Exception
	 */
	public String infos(OAuth oauth, String format, String names
			)
			throws Exception {
		List<QParameter> parameters = new ArrayList<QParameter>();
		parameters.add(new QParameter("format", format));
		parameters.add(new QParameter("names", names));
 
		return getResource("http://open.t.qq.com/api/user/infos",
				parameters, oauth);
	}	
}
