package com.yugioh.android.define;

public class NetworkDefine {

	public static final String BASE_URL = "http://diy.ourocg.cn/yugioh/";
	public static final String UPDATE_URL = BASE_URL + "update.php";
	public static final String UPDATE_PARAM_FMT = "ver=%d&cardid=%d&dbver=%d&os=a";
    public static final String FEEDBACK_URL = BASE_URL + "feedback.php";
    public static final String FEEDBACK_PARAM_FMT = "id=%s&email=%s&text=%s&appver=%d&osver=%d";

	public static final String RECOMMAND_URL = BASE_URL + "get_recommand.php";
	public static final String RECOMMAND_IMAGE_URL = BASE_URL + "recommand/";

	public static final String URL_APK = BASE_URL + "download/YuGiOhCard.apk";
	public static final String URL_DATA = BASE_URL + "download/yugioh.zip";

	public static final String URL_CARD_IMAGE_FMT = "http://p.ocgsoft.cn/%d.jpg";
    public static final String URL_OCGSOFT_BASE = "https://api.ourocg.cn/";
    public static final String URL_OCGSOFT_GET_PACKAGE = URL_OCGSOFT_BASE + "Package/list";
    public static final String URL_OCGSOFT_GET_PACKAGE_CARD = URL_OCGSOFT_BASE + "Package/card/packid/%s";

    public static final String URL_UPDATE_LOG = BASE_URL + "update.txt";
    public static final String URL_GITHUB = "https://github.com/rarnu/root-tools/tree/master/YGOCard";

}
