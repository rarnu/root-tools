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
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.CompoundButton.OnCheckedChangeListener;
import android.widget.Gallery;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.ScrollView;
import android.widget.TextView;
import android.widget.Toast;

import com.snda.gyue.adapter.ArticleItemAdapter;
import com.snda.gyue.adapter.ImageAdapter;
import com.snda.gyue.classes.ArticleItem;
import com.snda.gyue.network.HttpProxy;
import com.snda.gyue.network.ItemBuilder;
import com.snda.gyue.network.Updater;
import com.snda.gyue.utils.ImageUtils;
import com.snda.gyue.utils.MiscUtils;
import com.tencent.weibo.utils.Configuration;
import com.tencent.weibo.utils.Utils;

public class MainActivity extends Activity implements OnClickListener, OnItemClickListener, OnCheckedChangeListener {

	RelativeLayout btnFunc1, btnFunc2, btnFunc3, btnFunc4, btnFunc5;

	RelativeLayout layContent;
	ScrollView layMainFocus;
	ListView lvFocus, lvIndustry, lvApplication, lvGames;
	List<ArticleItem> lstFocus, lstIndustry, lstApplication, lstGames;
	ArticleItemAdapter adapterFocus, adapterIndustry, adapterApplication, adapterGames;
	ProgressBar pbRefreshing;
	Button btnRefresh;
	Gallery gallaryPhotos;
	RelativeLayout laySettings;
	TextView tvGName;

	CheckBox chkNoPic, chkOnlyWifi, chkShareWithPic;
	Button btnBindSinaWeibo, btnBindTencentWeibo, btnAbout;

	boolean loadedFocus = false, loadedIndustry = false, loadedApplication = false, loadedGames = false;
	int pageFocus = 1, pageIndustry = 1, pageApplication = 1, pageGames = 1;
	boolean hasNextFocus = true, hasNextIndustry = true, hasNextApplication = true, hasNextGames = true;
	boolean firstFocus = true, firstIndustry = true, firstApplication = true, firstGames = true;

	int CurrentType = 0;
	boolean inProgressFocus = false, inProgressIndustry = false, inProgressApplication = false, inProgressGames = false;
	Handler hUpdate;

	boolean starting = true;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

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
		layMainFocus = (ScrollView) findViewById(R.id.layMainFocus);
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

		chkNoPic = (CheckBox) findViewById(R.id.chkNoPic);
		chkOnlyWifi = (CheckBox) findViewById(R.id.chkOnlyWifi);
		chkShareWithPic = (CheckBox) findViewById(R.id.chkShareWithPic);
		btnBindSinaWeibo = (Button) findViewById(R.id.btnBindSinaWeibo);
		btnBindTencentWeibo = (Button) findViewById(R.id.btnBindTencentWeibo);
		btnAbout = (Button) findViewById(R.id.btnAbout);

		chkNoPic.setOnCheckedChangeListener(this);
		chkOnlyWifi.setOnCheckedChangeListener(this);
		chkShareWithPic.setOnCheckedChangeListener(this);
		btnBindSinaWeibo.setOnClickListener(this);
		btnBindTencentWeibo.setOnClickListener(this);
		btnAbout.setOnClickListener(this);
		readConfig();

		btnBindSinaWeibo.setText(GlobalInstance.sinaName.equals("") ? getString(R.string.bind_sina_weibo) : GlobalInstance.sinaName);
		btnBindTencentWeibo.setText(GlobalInstance.tencentName.equals("") ? getString(R.string.bind_tencent_weibo) : GlobalInstance.tencentName);

		pbRefreshing = (ProgressBar) findViewById(R.id.pbRefreshing);
		btnRefresh = (Button) findViewById(R.id.btnRefresh);
		tvGName = (TextView) findViewById(R.id.tvGName);
		gallaryPhotos = (Gallery) findViewById(R.id.gallaryPhotos);
		laySettings = (RelativeLayout) findViewById(R.id.laySettings);
		laySettings.setVisibility(View.GONE);

		btnRefresh.setOnClickListener(this);
		lvFocus.setOnItemClickListener(this);
		lvIndustry.setOnItemClickListener(this);
		lvApplication.setOnItemClickListener(this);
		lvGames.setOnItemClickListener(this);

		RelativeLayout.LayoutParams lp = (RelativeLayout.LayoutParams) gallaryPhotos.getLayoutParams();
		lp.height = (int) (260 * GlobalInstance.metric.widthPixels / 480);
		gallaryPhotos.setLayoutParams(lp);
		gallaryPhotos.setOnItemClickListener(this);

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
				btnBindSinaWeibo.setText(GlobalInstance.sinaName);
			}
		}

		if (bind.equals("tencent")) {
			writeConfig();
			if (!GlobalInstance.tencentName.equals("")) {
				btnBindTencentWeibo.setText(GlobalInstance.tencentName);
			}
		}
	}

	private void readConfig() {
		// read config
		SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(this);
		chkNoPic.setChecked(sp.getBoolean("nopic", false));
		chkOnlyWifi.setChecked(sp.getBoolean("onlywifi", false));
		chkShareWithPic.setChecked(sp.getBoolean("sharewithpic", true));

		GlobalInstance.shareWithPic = chkShareWithPic.isChecked();

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
		sp.edit().putBoolean("nopic", chkNoPic.isChecked()).putBoolean("onlywifi", chkOnlyWifi.isChecked())
				.putBoolean("sharewithpic", chkShareWithPic.isChecked()).putString("sinaToken", GlobalInstance.sinaToken)
				.putString("sinaSecret", GlobalInstance.sinaSecret).putString("tencentToken", GlobalInstance.tencentToken)
				.putString("tencentSecret", GlobalInstance.tencentSecret).putString("sinaName", GlobalInstance.sinaName)
				.putString("tencentName", GlobalInstance.tencentName).commit();

		GlobalInstance.shareWithPic = chkShareWithPic.isChecked();

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
						RelativeLayout.LayoutParams lp = (RelativeLayout.LayoutParams) lvFocus.getLayoutParams();
						loadedFocus = true;

						if (!hasNextFocus) {
							Toast.makeText(MainActivity.this, R.string.no_more, Toast.LENGTH_LONG).show();
						}
						setGalleryImages(lstFocus);

						List<ArticleItem> lstFocusTmp = null;
						if (lstFocus != null) {
							lstFocusTmp = new ArrayList<ArticleItem>(lstFocus);
							for (int i = 0; i < 5; i++) {
								lstFocusTmp.remove(0);
							}
						}

						if (lstFocusTmp == null) {
							adapterFocus = null;
						} else {
							adapterFocus = new ArticleItemAdapter(getLayoutInflater(), lstFocusTmp, lvFocus, gallaryPhotos);
						}
						lvFocus.setAdapter(adapterFocus);

						if (lstFocus == null) {
							lp.height = 0;
						} else {
							if (lstFocusTmp != null) {
								lp.height = ImageUtils.dipToPx(GlobalInstance.density, 97) * (lstFocusTmp.size() - 1)
										+ ImageUtils.dipToPx(GlobalInstance.density, 48);
							} else {
								lp.height = 0;
							}
						}

						lvFocus.setLayoutParams(lp);

						inProgressFocus = false;

						layMainFocus.post(new Runnable() {

							@Override
							public void run() {
								layMainFocus.scrollTo(0, 0);

							}
						});

						break;
					}
					case 13: {

						loadedIndustry = true;
						if (!hasNextIndustry) {
							Toast.makeText(MainActivity.this, R.string.no_more, Toast.LENGTH_LONG).show();
						}
						if (GlobalInstance.gListFocusedArticles == null || GlobalInstance.gListFocusedArticles.size() == 0) {
							if (lstIndustry == null) {
								GlobalInstance.gListFocusedArticles = null;
							} else {
								GlobalInstance.gListFocusedArticles = new ArrayList<ArticleItem>(lstIndustry);
							}
						}
						lvIndustry.setAdapter(adapterIndustry);
						lvIndustry.setSelection(pageIndustry - 2);
						inProgressIndustry = false;
						break;
					}
					case 11: {

						loadedApplication = true;
						if (!hasNextApplication) {
							Toast.makeText(MainActivity.this, R.string.no_more, Toast.LENGTH_LONG).show();
						}
						lvApplication.setAdapter(adapterApplication);
						lvApplication.setSelection(pageApplication - 2);

						inProgressApplication = false;
						break;
					}
					case 12: {

						loadedGames = true;
						if (!hasNextGames) {
							Toast.makeText(MainActivity.this, R.string.no_more, Toast.LENGTH_LONG).show();
						}
						lvGames.setAdapter(adapterGames);
						lvGames.setSelection(pageGames - 2);
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
								List<ArticleItem> tmp = ItemBuilder.xmlToItems(MainActivity.this, type, xml, (init ? local : false), false);
								if (tmp == null || tmp.size() == 0) {
									hasNextFocus = false;
								}
								mergeList(tmp, lstFocus, 50);
							}
						}
						if (lstFocus == null) {
							lstFocus = new ArrayList<ArticleItem>();
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
								List<ArticleItem> tmp = ItemBuilder.xmlToItems(MainActivity.this, type, xml, (init ? local : false), false);
								if (tmp == null || tmp.size() == 0) {
									hasNextIndustry = false;
								}
								mergeList(tmp, lstIndustry, -1);
							}
						}
						if (lstIndustry == null) {
							lstIndustry = new ArrayList<ArticleItem>();
						}
						addEmptyArticle(lstIndustry);

						adapterIndustry = new ArticleItemAdapter(getLayoutInflater(), lstIndustry, lvIndustry, null);
						break;
					case 11:
						if (page == 1) {
							lstApplication = ItemBuilder.xmlToItems(MainActivity.this, type, xml, (init ? local : false), true);
							pageApplication = 1;
							hasNextApplication = true;
						} else {
							if (hasNextApplication) {
								List<ArticleItem> tmp = ItemBuilder.xmlToItems(MainActivity.this, type, xml, (init ? local : false), false);
								if (tmp == null || tmp.size() == 0) {
									hasNextApplication = false;
								}
								mergeList(tmp, lstApplication, -1);
							}
						}
						if (lstApplication == null) {
							lstApplication = new ArrayList<ArticleItem>();
						}
						addEmptyArticle(lstApplication);

						adapterApplication = new ArticleItemAdapter(getLayoutInflater(), lstApplication, lvApplication, null);
						break;
					case 12:
						if (page == 1) {
							lstGames = ItemBuilder.xmlToItems(MainActivity.this, type, xml, (init ? local : false), true);
							pageGames = 1;
							hasNextGames = true;
						} else {
							if (hasNextGames) {
								List<ArticleItem> tmp = ItemBuilder.xmlToItems(MainActivity.this, type, xml, (init ? local : false), false);
								if (tmp == null || tmp.size() == 0) {
									hasNextGames = false;
								}
								mergeList(tmp, lstGames, -1);
							}
						}
						if (lstGames == null) {
							lstGames = new ArrayList<ArticleItem>();
						}
						addEmptyArticle(lstGames);
						adapterGames = new ArticleItemAdapter(getLayoutInflater(), lstGames, lvGames, null);
						break;
					}

				} catch (Exception e) {

				}
				h.sendEmptyMessage(1);

			}
		}).start();
	}

	private void mergeList(List<ArticleItem> source, List<ArticleItem> dest, int max) {
		dest.remove(dest.size() - 1);
		if (source != null && source.size() > 0) {
			for (ArticleItem item : source) {
				if (max != -1) {
					if (dest.size() >= max) {
						break;
					}
				}
				dest.add(item);
			}
		}
	}

	private void addEmptyArticle(List<ArticleItem> dest) {
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

	public void setGalleryImages(List<ArticleItem> images) {
		if (images == null) {
			return;
		}
		List<ArticleItem> list = new ArrayList<ArticleItem>();
		for (int i = 0; i < images.size(); i++) {
			if (images.get(i) == null) {
				continue;
			}
			if ((images.get(i).getArticleImageLocalFileName() != null) && (!images.get(i).getArticleImageLocalFileName().equals(""))) {
				list.add(images.get(i));
				if (list.size() >= 5) {
					break;
				}
			}
		}

		ImageAdapter imgAdapter = new ImageAdapter(this, getLayoutInflater(), list, lvFocus, gallaryPhotos);
		gallaryPhotos.setAdapter(imgAdapter);
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
			// case R.id.btnBack:
			// finish();
			// break;
			case R.id.btnRefresh:
				getArticleListT(CurrentType, 1, false);
				break;
			case R.id.btnBindSinaWeibo:

				if (GlobalInstance.sinaToken.equals("")) {
					// bind sina weibo
					Intent inSina = new Intent(this, BindSinaActivity.class);
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
									btnBindSinaWeibo.setText(R.string.bind_sina_weibo);

								}
							}).setNegativeButton(R.string.cancel, null).show();
				}
				break;
			case R.id.btnBindTencentWeibo:
				// bind tencent weibo
				if (GlobalInstance.tencentToken.equals("")) {
					Intent inTencent = new Intent(this, BindTencentActivity.class);
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
									btnBindTencentWeibo.setText(R.string.bind_tencent_weibo);

								}
							}).setNegativeButton(R.string.cancel, null).show();
				}
				break;
			case R.id.btnAbout:
				// show about dialog
				Intent inAbout = new Intent(this, AboutActivity.class);
				startActivity(inAbout);
				break;
			}
			return;
		}

		if (v instanceof RelativeLayout) {
			setSelectedItem((RelativeLayout) v);
		}

		layMainFocus.setVisibility(View.GONE);
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
			layMainFocus.setVisibility(View.VISIBLE);
			if (inProgressFocus) {
				btnRefresh.setEnabled(false);
				pbRefreshing.setVisibility(View.VISIBLE);
			}
			break;
		case R.id.btnFunc2:
			CurrentType = 13;
			tvGName.setText(R.string.func2_detail);
			lvIndustry.setVisibility(View.VISIBLE);
			if (inProgressIndustry) {
				btnRefresh.setEnabled(false);
				pbRefreshing.setVisibility(View.VISIBLE);
			}
			break;
		case R.id.btnFunc3:
			CurrentType = 11;
			tvGName.setText(R.string.func3_detail);
			lvApplication.setVisibility(View.VISIBLE);
			if (inProgressApplication) {
				btnRefresh.setEnabled(false);
				pbRefreshing.setVisibility(View.VISIBLE);
			}
			break;
		case R.id.btnFunc4:
			CurrentType = 12;
			tvGName.setText(R.string.func4_detail);
			lvGames.setVisibility(View.VISIBLE);
			if (inProgressGames) {
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
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode == RESULT_OK) {
			switch (requestCode) {
			case 1:
				writeConfig();
				if (!GlobalInstance.sinaName.equals("")) {
					btnBindSinaWeibo.setText(GlobalInstance.sinaName);
				}
				break;
			case 2:
				break;

			}
		}
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position, long id) {

		ArticleItem item = null;
		boolean needShowDownload = false;
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
				needShowDownload = true;
				item = (ArticleItem) lvApplication.getItemAtPosition(position);
				break;
			case R.id.lvGames:
				needShowDownload = true;
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
					view.findViewById(R.id.article_progress).setVisibility(View.VISIBLE);
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
					view.findViewById(R.id.article_progress).setVisibility(View.VISIBLE);
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
					view.findViewById(R.id.article_progress).setVisibility(View.VISIBLE);
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
					view.findViewById(R.id.article_progress).setVisibility(View.VISIBLE);
					getArticleListT(CurrentType, pageGames, false);
					break;
				}

				return;
			}

			break;
		}
		case R.id.gallaryPhotos:
			item = (ArticleItem) gallaryPhotos.getItemAtPosition(position);
			break;
		}

		if (item != null) {
			GlobalInstance.currentArticle = item;
			Intent inArticle = new Intent(this, ViewArticleActivity.class);
			inArticle.putExtra("no_pic", chkNoPic.isChecked());
			inArticle.putExtra("needShowDownload", needShowDownload);
			startActivity(inArticle);
		}

	}

	@Override
	public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
		if (!starting) {
			writeConfig();
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

}