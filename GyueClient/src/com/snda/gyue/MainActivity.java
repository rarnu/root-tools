package com.snda.gyue;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.Uri;
import android.net.wifi.WifiInfo;
import android.net.wifi.WifiManager;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.preference.PreferenceManager;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;
import android.widget.TextView;
import android.widget.Toast;

import com.snda.gyue.adapter.ArticleItemAdapter;
import com.snda.gyue.adapter.FocusItemAdapter;
import com.snda.gyue.adapter.ImageAdapter;
import com.snda.gyue.adapter.PreferenceAdapter;
import com.snda.gyue.classes.ArticleItem;
import com.snda.gyue.classes.SettingsItem;
import com.snda.gyue.component.GalleryFlow;
import com.snda.gyue.network.HttpProxy;
import com.snda.gyue.network.ItemBuilder;
import com.snda.gyue.network.Updater;
import com.snda.gyue.utils.ImageUtils;
import com.snda.gyue.utils.MiscUtils;
import com.snda.gyue.utils.UIUtils;
import com.tencent.weibo.utils.Configuration;
import com.tencent.weibo.utils.Utils;

public class MainActivity extends Activity implements OnClickListener, OnItemClickListener {

	RelativeLayout btnFunc1, btnFunc2, btnFunc3, btnFunc4, btnFunc5;

	RelativeLayout layContent;
	ListView lvFocus, lvIndustry, lvApplication, lvGames, lvSettings;
	List<Object> lstFocus, lstIndustry, lstApplication, lstGames;
	List<ArticleItem> lstGalleryItem;
	List<SettingsItem> lstPreference;
	ArticleItemAdapter adapterIndustry, adapterApplication, adapterGames;
	FocusItemAdapter adapterFocus;
	ProgressBar pbRefreshing;
	Button btnRefresh;
	GalleryFlow gallaryPhotos = null;
	ImageAdapter adapterGallery = null;
	PreferenceAdapter adapterPref = null;
	RelativeLayout laySettings;
	TextView tvGName;

	SettingsItem chkOnlyWifi, chkShareWithPic;
	SettingsItem btnBindSinaWeibo, btnBindTencentWeibo, btnAbout;

	boolean loadedFocus = false, loadedIndustry = false, loadedApplication = false, loadedGames = false;
	int pageFocus = 1, pageIndustry = 1, pageApplication = 1, pageGames = 1;
	boolean hasNextFocus = true, hasNextIndustry = true, hasNextApplication = true, hasNextGames = true;
	boolean firstFocus = true, firstIndustry = true, firstApplication = true, firstGames = true;

	int CurrentType = 0;
	boolean inProgressFocus = false, inProgressIndustry = false, inProgressApplication = false, inProgressGames = false;
	Handler hUpdate;

	boolean starting = true;
	boolean firstLoadFocus = true;

	public static final int ID_HEAD_GALLERY = 901;

	Handler hRefreshConfig = null;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		UIUtils.initDisplayMetrics(getWindowManager());

		starting = true;

		getWindowManager().getDefaultDisplay().getMetrics(GlobalInstance.metric);
		GlobalInstance.density = GlobalInstance.metric.density;

		if (!MiscUtils.sdcardExists()) {
			new AlertDialog.Builder(this).setTitle(R.string.hint).setMessage(R.string.sdcard_unmounted)
					.setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {

						@Override
						public void onClick(DialogInterface dialog, int which) {
							finish();
						}
					}).show();
			return;
		}

		Intent inSplash = new Intent(this, SplashActivity.class);
		startActivity(inSplash);

		setContentView(R.layout.main);

		layContent = (RelativeLayout) findViewById(R.id.layContent);
		btnFunc1 = (RelativeLayout) findViewById(R.id.btnFunc1);
		btnFunc2 = (RelativeLayout) findViewById(R.id.btnFunc2);
		btnFunc3 = (RelativeLayout) findViewById(R.id.btnFunc3);
		btnFunc4 = (RelativeLayout) findViewById(R.id.btnFunc4);
		btnFunc5 = (RelativeLayout) findViewById(R.id.btnFunc5);

		setIconText(btnFunc1, R.drawable.home, R.string.func1);
		setIconText(btnFunc2, R.drawable.news, R.string.func2);
		setIconText(btnFunc3, R.drawable.app, R.string.func3);
		setIconText(btnFunc4, R.drawable.game, R.string.func4);
		setIconText(btnFunc5, R.drawable.options, R.string.func5);

		lvFocus = (ListView) findViewById(R.id.lvFocus);
		lvIndustry = (ListView) findViewById(R.id.lvIndustry);
		lvApplication = (ListView) findViewById(R.id.lvApplication);
		lvGames = (ListView) findViewById(R.id.lvGames);
		lvSettings = (ListView) findViewById(R.id.lvSettings);

		readConfig();

		buildPreference();

		pbRefreshing = (ProgressBar) findViewById(R.id.pbRefreshing);
		btnRefresh = (Button) findViewById(R.id.btnRefresh);
		tvGName = (TextView) findViewById(R.id.tvGName);

		laySettings = (RelativeLayout) findViewById(R.id.laySettings);
		laySettings.setVisibility(View.GONE);

		btnRefresh.setOnClickListener(this);
		lvFocus.setOnItemClickListener(this);
		lvIndustry.setOnItemClickListener(this);
		lvApplication.setOnItemClickListener(this);
		lvGames.setOnItemClickListener(this);

		buildGalleryHead();

		adjustButtonWidth();

		initSelectedItem();

		onClick(btnFunc1);

		hUpdate = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 99) {
					new AlertDialog.Builder(MainActivity.this).setTitle(R.string.new_version).setMessage(R.string.new_version_desc)
							.setPositiveButton(R.string.update, new DialogInterface.OnClickListener() {

								@Override
								public void onClick(DialogInterface dialog, int which) {
									Intent inUpdate = new Intent(Intent.ACTION_VIEW);
									inUpdate.setData(Uri.parse(Updater.updateApk));
									startActivity(inUpdate);

								}
							}).setNegativeButton(R.string.cancel, null).show();
				}
				super.handleMessage(msg);
			}
		};

		Updater.checkUpdate(MainActivity.this, hUpdate);
		getIpAddress();
		starting = false;
	}

	private void buildPreference() {
		lstPreference = new ArrayList<SettingsItem>();

		chkOnlyWifi = new SettingsItem();
		chkOnlyWifi.setKey("onlywifi");
		chkOnlyWifi.setTitle(getString(R.string.only_wifi));
		chkOnlyWifi.setCheckBox(true);
		chkOnlyWifi.setChecked(GlobalInstance.onlyWifi);
		chkOnlyWifi.setSummary(getString(GlobalInstance.onlyWifi ? R.string.only_wifi_son : R.string.only_wifi_soff));
		lstPreference.add(chkOnlyWifi);

		chkShareWithPic = new SettingsItem();
		chkShareWithPic.setKey("sharewithpic");
		chkShareWithPic.setTitle(getString(R.string.share_with_pic));
		chkShareWithPic.setCheckBox(true);
		chkShareWithPic.setChecked(GlobalInstance.shareWithPic);
		chkShareWithPic.setSummary(getString(GlobalInstance.shareWithPic ? R.string.share_with_pic_son : R.string.share_with_pic_soff));
		lstPreference.add(chkShareWithPic);

		btnBindSinaWeibo = new SettingsItem();
		btnBindSinaWeibo.setKey("bind_sina");
		btnBindSinaWeibo.setCheckBox(false);
		btnBindSinaWeibo.setIcon(R.drawable.sina_logo);
		btnBindSinaWeibo.setTitle(getString(GlobalInstance.sinaName.equals("") ? R.string.bind_sina_weibo : R.string.unbind_sina_weibo));
		btnBindSinaWeibo.setSummary(GlobalInstance.sinaName.equals("") ? getString(R.string.bind_sina_sno) : String.format(getString(R.string.bind_sina_fmt),
				GlobalInstance.sinaName));
		lstPreference.add(btnBindSinaWeibo);

		btnBindTencentWeibo = new SettingsItem();
		btnBindTencentWeibo.setKey("bind_tencent");
		btnBindTencentWeibo.setCheckBox(false);
		btnBindTencentWeibo.setIcon(R.drawable.tencent_logo);
		btnBindTencentWeibo.setTitle(getString(GlobalInstance.tencentName.equals("") ? R.string.bind_tencent_weibo : R.string.unbind_tencent_weibo));
		btnBindTencentWeibo.setSummary(GlobalInstance.tencentName.equals("") ? getString(R.string.bind_tencent_sno) : String.format(
				getString(R.string.bind_tencent_fmt), GlobalInstance.tencentName));
		lstPreference.add(btnBindTencentWeibo);

		btnAbout = new SettingsItem();
		btnAbout.setKey("about");
		btnAbout.setCheckBox(false);
		btnAbout.setTitle(getString(R.string.about));
		lstPreference.add(btnAbout);

		hRefreshConfig = new Handler() {
			
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 101) {
					GlobalInstance.onlyWifi = chkOnlyWifi.isChecked();
					GlobalInstance.shareWithPic = chkShareWithPic.isChecked();
					writeConfig();
				}
				super.handleMessage(msg);
			}
			
		};
		
		adapterPref = new PreferenceAdapter(getLayoutInflater(), lstPreference, hRefreshConfig);
		lvSettings.setAdapter(adapterPref);

		lvSettings.setOnItemClickListener(this);
	}

	private void buildGalleryHead() {
		if (gallaryPhotos == null) {

			RelativeLayout layGallary = new RelativeLayout(this);

			gallaryPhotos = new GalleryFlow(this);
			gallaryPhotos.setId(ID_HEAD_GALLERY);
			gallaryPhotos.setClipChildren(true);
			gallaryPhotos.setOnItemClickListener(this);
			RelativeLayout.LayoutParams lpHead = new RelativeLayout.LayoutParams(LayoutParams.MATCH_PARENT, UIUtils.dipToPx(160));
			lpHead.topMargin = UIUtils.dipToPx(4);
			lpHead.bottomMargin = UIUtils.dipToPx(4);
			gallaryPhotos.setLayoutParams(lpHead);
			layGallary.addView(gallaryPhotos);
			lvFocus.addHeaderView(layGallary);

		}
	}

	private void updateGallery() {
		// update gallery
		if (lstGalleryItem == null) {
			lstGalleryItem = new ArrayList<ArticleItem>();
		}
		lstGalleryItem.clear();
		for (int i = 0; i < 5; i++) {
			lstGalleryItem.add((ArticleItem) lstFocus.get(i));
		}

		if (adapterGallery == null) {
			adapterGallery = new ImageAdapter(this, getLayoutInflater(), lstGalleryItem, lvFocus, gallaryPhotos);
			gallaryPhotos.setAdapter(adapterGallery);
		}

		if (firstLoadFocus) {
			firstLoadFocus = false;
			gallaryPhotos.setSelection(2);
		}
	}

	private void getIpAddress() {
		WifiManager wifiManager = (WifiManager) getSystemService(WIFI_SERVICE);
		WifiInfo wifiInfo = wifiManager.getConnectionInfo();
		int ipAddress = wifiInfo.getIpAddress();
		Configuration.wifiIp = Utils.intToIp(ipAddress);
	}

	@Override
	protected void onNewIntent(Intent intent) {
		super.onNewIntent(intent);
		if (intent == null) {
			return;
		}
		String bind = intent.getStringExtra("bind");

		if (bind == null) {
			return;
		}

		if (bind.equals("sina")) {
			writeConfig();
			if (!GlobalInstance.sinaName.equals("")) {
				// btnBindSinaWeibo.setText(GlobalInstance.sinaName);
				btnBindSinaWeibo.setTitle(getString(GlobalInstance.sinaName.equals("") ? R.string.bind_sina_weibo : R.string.unbind_sina_weibo));
				btnBindSinaWeibo.setSummary(GlobalInstance.sinaName.equals("") ? getString(R.string.bind_sina_sno) : String.format(
						getString(R.string.bind_sina_fmt), GlobalInstance.sinaName));
				if (adapterPref != null) {
					adapterPref.notifyDataSetChanged();
				}
			}
		}

		if (bind.equals("tencent")) {
			writeConfig();
			if (!GlobalInstance.tencentName.equals("")) {
				// btnBindTencentWeibo.setText(GlobalInstance.tencentName);
				btnBindTencentWeibo.setTitle(getString(GlobalInstance.tencentName.equals("") ? R.string.bind_tencent_weibo : R.string.unbind_tencent_weibo));
				btnBindTencentWeibo.setSummary(GlobalInstance.tencentName.equals("") ? getString(R.string.bind_tencent_sno) : String.format(
						getString(R.string.bind_tencent_fmt), GlobalInstance.tencentName));
				if (adapterPref != null) {
					adapterPref.notifyDataSetChanged();
				}
			}
		}
	}

	private void readConfig() {
		// read config
		SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(this);
		GlobalInstance.onlyWifi = sp.getBoolean("onlywifi", false);
		GlobalInstance.shareWithPic = sp.getBoolean("sharewithpic", true);

		GlobalInstance.sinaToken = sp.getString("sinaToken", "");
		GlobalInstance.sinaSecret = sp.getString("sinaSecret", "");
		GlobalInstance.tencentToken = sp.getString("tencentToken", "");
		GlobalInstance.tencentSecret = sp.getString("tencentSecret", "");
		GlobalInstance.sinaName = sp.getString("sinaName", "");
		GlobalInstance.tencentName = sp.getString("tencentName", "");
	}

	private void writeConfig() {
		// write config
		SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(this);
		sp.edit().putBoolean("onlywifi", GlobalInstance.onlyWifi).putBoolean("sharewithpic", GlobalInstance.shareWithPic)
				.putString("sinaToken", GlobalInstance.sinaToken).putString("sinaSecret", GlobalInstance.sinaSecret)
				.putString("tencentToken", GlobalInstance.tencentToken).putString("tencentSecret", GlobalInstance.tencentSecret)
				.putString("sinaName", GlobalInstance.sinaName).putString("tencentName", GlobalInstance.tencentName).commit();
	}

	private void getArticleListT(final int type, final int page, final boolean local) {

		final File fTmp = new File(GyueConsts.GYUE_DIR + String.format("a%d.xml", type));
		final boolean init = fTmp.exists();

		btnRefresh.setEnabled(true);
		pbRefreshing.setVisibility(View.GONE);
		switch (type) {
		case 54:
			inProgressFocus = true;
			break;
		case 13:
			inProgressIndustry = true;
			break;
		case 11:
			inProgressApplication = true;
			break;
		case 12:
			inProgressGames = true;
			break;
		}

		if (CurrentType == type) {
			btnRefresh.setEnabled(false);
			pbRefreshing.setVisibility(View.VISIBLE);
		}

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {

					switch (type) {
					case 54: {
						loadedFocus = true;
						if (!hasNextFocus) {
							Toast.makeText(MainActivity.this, R.string.no_more, Toast.LENGTH_LONG).show();
						}

						if (adapterFocus == null) {
							adapterFocus = new FocusItemAdapter(getLayoutInflater(), lstFocus, lvFocus, 54);
						}

						if (lvFocus.getAdapter() == null) {
							lvFocus.setAdapter(adapterFocus);
						}
						adapterFocus.setNewList(lstFocus);
						updateGallery();
						inProgressFocus = false;
						break;
					}
					case 13: {

						loadedIndustry = true;
						if (!hasNextIndustry) {
							Toast.makeText(MainActivity.this, R.string.no_more, Toast.LENGTH_LONG).show();
						}
						if (lvIndustry.getAdapter() == null) {
							lvIndustry.setAdapter(adapterIndustry);
						}
						adapterIndustry.setNewList(lstIndustry);
						inProgressIndustry = false;
						break;
					}
					case 11: {

						loadedApplication = true;
						if (!hasNextApplication) {
							Toast.makeText(MainActivity.this, R.string.no_more, Toast.LENGTH_LONG).show();
						}

						if (lvApplication.getAdapter() == null) {
							lvApplication.setAdapter(adapterApplication);
						}
						adapterApplication.setNewList(lstApplication);

						inProgressApplication = false;
						break;
					}
					case 12: {

						loadedGames = true;
						if (!hasNextGames) {
							Toast.makeText(MainActivity.this, R.string.no_more, Toast.LENGTH_LONG).show();
						}
						if (lvGames.getAdapter() == null) {
							lvGames.setAdapter(adapterGames);
						}
						adapterGames.setNewList(lstGames);

						inProgressGames = false;
						break;
					}
					}

					if (CurrentType == type) {
						btnRefresh.setEnabled(true);
						pbRefreshing.setVisibility(View.GONE);
					}

					switch (type) {
					case 54:
						if (firstFocus) {
							firstFocus = false;
							if (MiscUtils.getNetworkType(MainActivity.this) != 0) {
								if (chkOnlyWifi.isChecked()) {
									if (MiscUtils.getNetworkType(MainActivity.this) == 1) {
										getArticleListT(type, 1, false);
									}
								} else {
									getArticleListT(type, 1, false);
								}
							}
						}
						break;
					case 13:
						if (firstIndustry) {
							firstIndustry = false;
							if (MiscUtils.getNetworkType(MainActivity.this) != 0) {
								if (chkOnlyWifi.isChecked()) {
									if (MiscUtils.getNetworkType(MainActivity.this) == 1) {
										getArticleListT(type, 1, false);
									}
								} else {
									getArticleListT(type, 1, false);
								}
							}
						}
						break;
					case 11:
						if (firstApplication) {
							firstApplication = false;
							if (MiscUtils.getNetworkType(MainActivity.this) != 0) {
								if (chkOnlyWifi.isChecked()) {
									if (MiscUtils.getNetworkType(MainActivity.this) == 1) {
										getArticleListT(type, 1, false);
									}
								} else {
									getArticleListT(type, 1, false);
								}
							}
						}
						break;
					case 12:
						if (firstGames) {
							firstGames = false;
							if (MiscUtils.getNetworkType(MainActivity.this) != 0) {
								if (chkOnlyWifi.isChecked()) {
									if (MiscUtils.getNetworkType(MainActivity.this) == 1) {
										getArticleListT(type, 1, false);
									}
								} else {
									getArticleListT(type, 1, false);
								}
							}
						}
						break;
					}

				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {

				try {
					String xml = "";
					if ((!local) || (!init)) {
						xml = HttpProxy.CallGet(GyueConsts.SITE_URL, String.format(GyueConsts.REQ_PARAMS, type, page, GyueConsts.PAGE_SIZE), "GBK");
					}
					switch (type) {
					case 54:
						if (page == 1) {
							lstFocus = ItemBuilder.xmlToItems(MainActivity.this, type, xml, (init ? local : false), true);
							pageFocus = 1;
							hasNextFocus = true;
						} else {
							if (hasNextFocus) {
								List<Object> tmp = ItemBuilder.xmlToItems(MainActivity.this, type, xml, (init ? local : false), false);
								if (tmp == null || tmp.size() == 0) {
									hasNextFocus = false;
								}
								mergeList(tmp, lstFocus, -1);
							}
						}
						if (lstFocus == null) {
							lstFocus = new ArrayList<Object>();
						}
						addEmptyArticle(lstFocus);

						break;
					case 13:
						if (page == 1) {
							lstIndustry = ItemBuilder.xmlToItems(MainActivity.this, type, xml, (init ? local : false), true);
							pageIndustry = 1;
							hasNextIndustry = true;
						} else {
							if (hasNextIndustry) {
								List<Object> tmp = ItemBuilder.xmlToItems(MainActivity.this, type, xml, (init ? local : false), false);
								if (tmp == null || tmp.size() == 0) {
									hasNextIndustry = false;
								}
								mergeList(tmp, lstIndustry, -1);
							}
						}
						if (lstIndustry == null) {
							lstIndustry = new ArrayList<Object>();
						}
						addEmptyArticle(lstIndustry);

						if (adapterIndustry == null) {
							adapterIndustry = new ArticleItemAdapter(getLayoutInflater(), lstIndustry, lvIndustry, null, 13);
						}
						break;
					case 11:
						if (page == 1) {
							lstApplication = ItemBuilder.xmlToItems(MainActivity.this, type, xml, (init ? local : false), true);
							pageApplication = 1;
							hasNextApplication = true;
						} else {
							if (hasNextApplication) {
								List<Object> tmp = ItemBuilder.xmlToItems(MainActivity.this, type, xml, (init ? local : false), false);
								if (tmp == null || tmp.size() == 0) {
									hasNextApplication = false;
								}
								mergeList(tmp, lstApplication, -1);
							}
						}
						if (lstApplication == null) {
							lstApplication = new ArrayList<Object>();
						}
						addEmptyArticle(lstApplication);

						if (adapterApplication == null) {
							adapterApplication = new ArticleItemAdapter(getLayoutInflater(), lstApplication, lvApplication, null, 11);
						}
						break;
					case 12:
						if (page == 1) {
							lstGames = ItemBuilder.xmlToItems(MainActivity.this, type, xml, (init ? local : false), true);
							pageGames = 1;
							hasNextGames = true;
						} else {
							if (hasNextGames) {
								List<Object> tmp = ItemBuilder.xmlToItems(MainActivity.this, type, xml, (init ? local : false), false);
								if (tmp == null || tmp.size() == 0) {
									hasNextGames = false;
								}
								mergeList(tmp, lstGames, -1);
							}
						}
						if (lstGames == null) {
							lstGames = new ArrayList<Object>();
						}
						addEmptyArticle(lstGames);
						if (adapterGames == null) {
							adapterGames = new ArticleItemAdapter(getLayoutInflater(), lstGames, lvGames, null, 12);
						}
						break;
					}

				} catch (Exception e) {

				}
				h.sendEmptyMessage(1);

			}
		}).start();
	}

	private void mergeList(List<Object> source, List<Object> dest, int max) {
		dest.remove(dest.size() - 1);
		if (source != null && source.size() > 0) {
			for (Object item : source) {
				if (max != -1) {
					if (dest.size() >= max) {
						break;
					}
				}
				dest.add(item);
			}
		}
	}

	private void addEmptyArticle(List<Object> dest) {
		ArticleItem item = new ArticleItem();
		item.setTitle("0");
		dest.add(item);
	}

	private void adjustButtonWidth() {

		int wid = (getWindowManager().getDefaultDisplay().getWidth() - ImageUtils.dipToPx(GlobalInstance.density, 40)) / 5;
		setButtonWidth(btnFunc1, wid);
		setButtonWidth(btnFunc2, wid);
		setButtonWidth(btnFunc3, wid);
		setButtonWidth(btnFunc4, wid);
		setButtonWidth(btnFunc5, wid);

	}

	private void setButtonWidth(RelativeLayout btn, int width) {
		LinearLayout.LayoutParams lp = (LinearLayout.LayoutParams) btn.getLayoutParams();
		lp.width = width;
		btn.setLayoutParams(lp);
	}

	private void setIconText(RelativeLayout btn, int icon, int text) {
		((ImageView) btn.findViewById(R.id.imgItemIco)).setBackgroundDrawable(getResources().getDrawable(icon));
		((TextView) btn.findViewById(R.id.tvItemName)).setText(text);
		btn.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {

		if (v instanceof Button) {
			switch (v.getId()) {

			case R.id.btnRefresh:
				int nt = MiscUtils.getNetworkType(this);
				if (nt == 0) {
					Toast.makeText(this, R.string.no_network, Toast.LENGTH_LONG).show();
					return;
				}

				switch (CurrentType) {
				case 54:
					if (adapterFocus != null) {
						adapterFocus.setUpdateStatus(true);
					}
					break;
				case 13:
					if (adapterIndustry != null) {
						adapterIndustry.setUpdateStatus(true);
					}
					break;
				case 11:
					if (adapterApplication != null) {
						adapterApplication.setUpdateStatus(true);
					}
					break;
				case 12:
					if (adapterGames != null) {
						adapterGames.setUpdateStatus(true);
					}
					break;
				}
				getArticleListT(CurrentType, 1, false);
				return;
			}
			return;
		}

		if (v instanceof RelativeLayout) {
			setSelectedItem((RelativeLayout) v);
		}

		lvFocus.setVisibility(View.GONE);
		lvIndustry.setVisibility(View.GONE);
		lvApplication.setVisibility(View.GONE);
		lvGames.setVisibility(View.GONE);
		laySettings.setVisibility(View.GONE);
		btnRefresh.setVisibility(View.VISIBLE);
		btnRefresh.setEnabled(true);
		pbRefreshing.setVisibility(View.GONE);
		switch (v.getId()) {
		case R.id.btnFunc1:
			CurrentType = 54;
			tvGName.setText(R.string.func1_detail);
			lvFocus.setVisibility(View.VISIBLE);
			if (inProgressFocus) {
				if (adapterFocus != null) {
					adapterFocus.setUpdateStatus(true);
				}
				btnRefresh.setEnabled(false);
				pbRefreshing.setVisibility(View.VISIBLE);
			}
			break;
		case R.id.btnFunc2:
			CurrentType = 13;
			tvGName.setText(R.string.func2_detail);
			lvIndustry.setVisibility(View.VISIBLE);
			if (inProgressIndustry) {
				if (adapterIndustry != null) {
					adapterIndustry.setUpdateStatus(true);
				}
				btnRefresh.setEnabled(false);
				pbRefreshing.setVisibility(View.VISIBLE);
			}
			break;
		case R.id.btnFunc3:
			CurrentType = 11;
			tvGName.setText(R.string.func3_detail);
			lvApplication.setVisibility(View.VISIBLE);
			if (inProgressApplication) {
				if (adapterApplication != null) {
					adapterApplication.setUpdateStatus(true);
				}
				btnRefresh.setEnabled(false);
				pbRefreshing.setVisibility(View.VISIBLE);
			}
			break;
		case R.id.btnFunc4:
			CurrentType = 12;
			tvGName.setText(R.string.func4_detail);
			lvGames.setVisibility(View.VISIBLE);
			if (inProgressGames) {
				if (adapterGames != null) {
					adapterGames.setUpdateStatus(true);
				}
				btnRefresh.setEnabled(false);
				pbRefreshing.setVisibility(View.VISIBLE);
			}
			break;
		case R.id.btnFunc5:
			tvGName.setText(R.string.func5_detail);
			btnRefresh.setVisibility(View.GONE);
			laySettings.setVisibility(View.VISIBLE);
			return;
		}

		switch (CurrentType) {
		case 54:
			if (loadedFocus) {
				return;
			}
			break;
		case 13:
			if (loadedIndustry) {
				return;
			}
			break;
		case 11:
			if (loadedApplication) {
				return;
			}
			break;
		case 12:
			if (loadedGames) {
				return;
			}
			break;
		}
		getArticleListT(CurrentType, 1, true);
	}

	private void initSelectedItem() {
		btnFunc1.setBackgroundDrawable(getResources().getDrawable(R.drawable.item_focus));
		btnFunc2.setBackgroundDrawable(getResources().getDrawable(R.drawable.item_focus));
		btnFunc3.setBackgroundDrawable(getResources().getDrawable(R.drawable.item_focus));
		btnFunc4.setBackgroundDrawable(getResources().getDrawable(R.drawable.item_focus));
		btnFunc5.setBackgroundDrawable(getResources().getDrawable(R.drawable.item_focus));
	}

	private void setSelectedItem(RelativeLayout btn) {
		btnFunc1.setBackgroundDrawable(null);
		btnFunc2.setBackgroundDrawable(null);
		btnFunc3.setBackgroundDrawable(null);
		btnFunc4.setBackgroundDrawable(null);
		btnFunc5.setBackgroundDrawable(null);
		btn.setBackgroundDrawable(getResources().getDrawable(R.drawable.item_focus));
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position, long id) {

		ArticleItem item = null;
		switch (parent.getId()) {
		case R.id.lvFocus:
		case R.id.lvIndustry:
		case R.id.lvApplication:
		case R.id.lvGames: {

			switch (parent.getId()) {
			case R.id.lvFocus:
				item = (ArticleItem) lvFocus.getItemAtPosition(position);
				break;
			case R.id.lvIndustry:
				item = (ArticleItem) lvIndustry.getItemAtPosition(position);
				break;
			case R.id.lvApplication:
				item = (ArticleItem) lvApplication.getItemAtPosition(position);
				break;
			case R.id.lvGames:
				item = (ArticleItem) lvGames.getItemAtPosition(position);
				break;
			}

			if (item.getTitle().equals("0")) {

				int nt = MiscUtils.getNetworkType(this);
				if (nt == 0) {
					Toast.makeText(this, R.string.no_network, Toast.LENGTH_LONG).show();
					return;
				}

				if (chkOnlyWifi.isChecked()) {
					if (nt != 1) {
						Toast.makeText(this, R.string.only_wifi_refresh, Toast.LENGTH_LONG).show();
						return;
					}
				}

				switch (CurrentType) {
				case 54:
					if (!hasNextFocus) {
						Toast.makeText(this, R.string.no_more, Toast.LENGTH_LONG).show();
						return;
					}
					if (inProgressFocus) {
						return;
					}
					pageFocus += GyueConsts.PAGE_SIZE;
					adapterFocus.setUpdateStatus(true);
					getArticleListT(CurrentType, pageFocus, false);
					break;
				case 13:
					if (!hasNextIndustry) {
						Toast.makeText(this, R.string.no_more, Toast.LENGTH_LONG).show();
						return;
					}
					if (inProgressIndustry) {
						return;
					}
					pageIndustry += GyueConsts.PAGE_SIZE;
					adapterIndustry.setUpdateStatus(true);
					getArticleListT(CurrentType, pageIndustry, false);
					break;
				case 11:
					if (!hasNextApplication) {
						Toast.makeText(this, R.string.no_more, Toast.LENGTH_LONG).show();
						return;
					}
					if (inProgressApplication) {
						return;
					}
					pageApplication += GyueConsts.PAGE_SIZE;
					adapterApplication.setUpdateStatus(true);
					getArticleListT(CurrentType, pageApplication, false);
					break;
				case 12:
					if (!hasNextGames) {
						Toast.makeText(this, R.string.no_more, Toast.LENGTH_LONG).show();
						return;
					}
					if (inProgressGames) {
						return;
					}
					pageGames += GyueConsts.PAGE_SIZE;
					adapterGames.setUpdateStatus(true);
					getArticleListT(CurrentType, pageGames, false);
					break;
				}
				return;
			}

			break;
		}
		case ID_HEAD_GALLERY:
			item = (ArticleItem) gallaryPhotos.getItemAtPosition(position);
			break;
		case R.id.lvSettings:
			
			SettingsItem sitem = (SettingsItem) lvSettings.getItemAtPosition(position);
			if (sitem.getKey().equals("bind_sina")) {
				if (GlobalInstance.sinaToken.equals("")) {
					// bind sina weibo
					Intent inSina = new Intent(this, BeforeBindActivity.class);
					inSina.putExtra("auth", 1);
					startActivity(inSina);
				} else {
					new AlertDialog.Builder(this).setTitle(R.string.hint).setMessage(R.string.unbind_sina)
							.setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {

								@Override
								public void onClick(DialogInterface dialog, int which) {
									GlobalInstance.sinaName = "";
									GlobalInstance.sinaToken = "";
									GlobalInstance.sinaSecret = "";
									writeConfig();
									btnBindSinaWeibo.setTitle(getString(R.string.bind_sina_weibo));
									btnBindSinaWeibo.setSummary(getString(R.string.bind_sina_sno));
									adapterPref.notifyDataSetChanged();
								}
							}).setNegativeButton(R.string.cancel, null).show();
				}
			} else if (sitem.getKey().equals("bind_tencent")) {
				if (GlobalInstance.tencentToken.equals("")) {
					Intent inTencent = new Intent(this, BeforeBindActivity.class);
					inTencent.putExtra("auth", 2);
					startActivity(inTencent);
				} else {
					new AlertDialog.Builder(this).setTitle(R.string.hint).setMessage(R.string.unbind_tencent)
							.setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {

								@Override
								public void onClick(DialogInterface dialog, int which) {
									GlobalInstance.tencentName = "";
									GlobalInstance.tencentToken = "";
									GlobalInstance.tencentSecret = "";
									writeConfig();
									btnBindTencentWeibo.setTitle(getString(R.string.bind_tencent_weibo));
									btnBindTencentWeibo.setSummary(getString(R.string.bind_tencent_sno));
									adapterPref.notifyDataSetChanged();
								}
							}).setNegativeButton(R.string.cancel, null).show();
				}
			} else if (sitem.getKey().equals("about")) {
				Intent inAbout = new Intent(this, AboutActivity.class);
				startActivity(inAbout);
			}

			return;
		}

		if (item != null) {
			GlobalInstance.currentArticle = item;
			Intent inArticle = new Intent(this, ViewArticleActivity.class);
			startActivity(inArticle);
		}

	}

	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		if (keyCode == KeyEvent.KEYCODE_BACK) {
			new AlertDialog.Builder(this).setTitle(R.string.hint).setMessage(R.string.close_confirm)
					.setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {

						@Override
						public void onClick(DialogInterface dialog, int which) {
							finish();

						}
					}).setNegativeButton(R.string.cancel, null).show();
			return true;
		} else {
			return super.onKeyDown(keyCode, event);
		}
	}

	@Override
	protected void onResume() {
		super.onResume();
		if (adapterFocus != null) {
			adapterFocus.notifyDataSetChanged();
		}
		if (adapterIndustry != null) {
			adapterIndustry.notifyDataSetChanged();
		}
		if (adapterApplication != null) {
			adapterApplication.notifyDataSetChanged();
		}
		if (adapterGames != null) {
			adapterGames.notifyDataSetChanged();
		}

	}
}