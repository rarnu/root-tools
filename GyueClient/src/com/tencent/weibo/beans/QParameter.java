package com.tencent.weibo.beans;


/**
 * OAuth参数构建类
 * 
 * @author <a href="http://blog.javawind.net">Xuefang Xu</a> 
 * @Data 2010-01-21
 * @Version 1.0.0
 */

public class QParameter implements java.io.Serializable, Comparable<QParameter> {

	/**
	 *  
	 */
	private static final long serialVersionUID = -9041723304674844461L;
	private String name;
	private String value;

	public QParameter(String name, String value) {
		this.name = name;
		this.value = value;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	@Override
	public int compareTo(QParameter param) {
		int compared;
		compared = this.name.compareTo(param.getName());
		if (0 == compared) {
			compared = this.value.compareTo(param.getValue());
		}
		return compared;
	}
}
