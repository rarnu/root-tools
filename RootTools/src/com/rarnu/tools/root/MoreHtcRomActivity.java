package com.rarnu.tools.root;

import android.app.Fragment;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Toast;

import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.comp.HtcRomItem;
import com.rarnu.tools.root.utils.ApkUtils;


public class MoreHtcRomActivity extends BaseActivity implements OnClickListener {

	HtcRomItem itmCar, itmFacebook, itmTwitter, itmDropbox, itmSkydrive,
			itmLaputa, itmFlickr, itmFriendStream, itmGoogle, itm3rd;
	DataProgressBar progressHtcRom;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_more_htcrom);

	}

	
	public void init() {

		mappingComp();


		itmCar.setName(R.string.itm_car);
		itmCar.setDesc(R.string.itmdesc_car);
		itmFacebook.setName(R.string.itm_facebook);
		itmFacebook.setDesc(R.string.itmdesc_facebook);
		itmTwitter.setName(R.string.itm_twitter);
		itmTwitter.setDesc(R.string.itmdesc_twitter);
		itmDropbox.setName(R.string.itm_dropbox);
		itmDropbox.setDesc(R.string.itmdesc_dropbox);
		itmSkydrive.setName(R.string.itm_skydrive);
		itmSkydrive.setDesc(R.string.itmdesc_skydrive);
		itmLaputa.setName(R.string.itm_laputa);
		itmLaputa.setDesc(R.string.itmdesc_laputa);
		itmFlickr.setName(R.string.itm_flickr);
		itmFlickr.setDesc(R.string.itmdesc_flickr);
		itmFriendStream.setName(R.string.itm_friendstream);
		itmFriendStream.setDesc(R.string.itmdesc_friendstream);
		itmGoogle.setName(R.string.itm_google);
		itmGoogle.setDesc(R.string.itmdesc_google);
		itm3rd.setName(R.string.itm_3rd);
		itm3rd.setDesc(R.string.itmdesc_3rd);
	}

	
	public void mappingComp() {
		progressHtcRom = (DataProgressBar) findViewById(R.id.progressHtcRom);
		itmCar = (HtcRomItem) findViewById(R.id.itmCar);
		itmFacebook = (HtcRomItem) findViewById(R.id.itmFacebook);
		itmTwitter = (HtcRomItem) findViewById(R.id.itmTwitter);
		itmDropbox = (HtcRomItem) findViewById(R.id.itmDropbox);
		itmSkydrive = (HtcRomItem) findViewById(R.id.itmSkydrive);
		itmLaputa = (HtcRomItem) findViewById(R.id.itmLaputa);
		itmFlickr = (HtcRomItem) findViewById(R.id.itmFlickr);
		itmFriendStream = (HtcRomItem) findViewById(R.id.itmFriendStream);
		itmGoogle = (HtcRomItem) findViewById(R.id.itmGoogle);
		itm3rd = (HtcRomItem) findViewById(R.id.itm3rd);
	}


	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			break;
		case R.id.btnRight:
			cleanHtcRom();
			break;
		}

	}

	private void cleanHtcRom() {
		if (!itmCar.isChecked() && !itmFacebook.isChecked()
				&& !itmTwitter.isChecked() && !itmDropbox.isChecked()
				&& !itmSkydrive.isChecked() && !itmLaputa.isChecked()
				&& !itmFlickr.isChecked() && !itmFriendStream.isChecked()
				&& !itmGoogle.isChecked() && !itm3rd.isChecked()) {
			Toast.makeText(this, R.string.no_clean_item_selected,
					Toast.LENGTH_LONG).show();
			return;
		}

		AlertDialogEx.showAlertDialogEx(this,
				getString(R.string.clean_htc_rom),
				getString(R.string.clean_htc_rom_confirm),
				getString(R.string.ok),
				new AlertDialogEx.DialogButtonClickListener() {

					@Override
					public void onClick(View v) {
						doCleanRom();

					}
				}, getString(R.string.cancel), null);
	}

	private void doCleanRom() {
		progressHtcRom.setVisibility(View.VISIBLE);
		progressHtcRom.setAppName(getString(R.string.cleaning_htcrom));
		itmCar.disable();
		itmFacebook.disable();
		itmTwitter.disable();
		itmDropbox.disable();
		itmSkydrive.disable();
		itmLaputa.disable();
		itmFlickr.disable();
		itmFriendStream.disable();
		itmGoogle.disable();
		itm3rd.disable();

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					progressHtcRom.setVisibility(View.GONE);
					itmCar.enable();
					itmFacebook.enable();
					itmTwitter.enable();
					itmDropbox.enable();
					itmSkydrive.enable();
					itmLaputa.enable();
					itmFlickr.enable();
					itmFriendStream.enable();
					itmGoogle.enable();
					itm3rd.enable();

					itmCar.setChecked(false);
					itmFacebook.setChecked(false);
					itmTwitter.setChecked(false);
					itmDropbox.setChecked(false);
					itmSkydrive.setChecked(false);
					itmLaputa.setChecked(false);
					itmFlickr.setChecked(false);
					itmFriendStream.setChecked(false);
					itmGoogle.setChecked(false);
					itm3rd.setChecked(false);
				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {

				if (itmCar.isChecked()) {
					deleteApplication("com.htc.AutoMotive");
					deleteApplication("com.htc.AutoMotive.Traffic");
					deleteApplication("com.htc.InternetRadio");
					deleteApplication("com.htc.autobot.cargps.provider");
				}

				if (itmFacebook.isChecked()) {
					deleteApplication("com.htc.socialnetwork.facebook");
					deleteApplication("com.htc.engine.facebook");
					deleteApplication("com.facebook.katana");
				}

				if (itmTwitter.isChecked()) {
					deleteApplication("com.htc.htctwitter");
					deleteApplication("com.htc.engine.twitter");
					deleteApplication("com.htc.Twitter3DWidget");
					deleteApplication("com.htc.Trends3DWidget");
					deleteApplication("com.twitter.android");
				}
				if (itmDropbox.isChecked()) {
					deleteApplication("com.htc.dropbox.glrplugin");
					deleteApplication("com.htc.cloudstorage.dropbox");
					deleteApplication("com.dropbox.android");
				}
				if (itmSkydrive.isChecked()) {
					deleteApplication("com.htc.skydrive.glrplugin");
					deleteApplication("com.htc.cloudstorage.skydrive");
				}
				if (itmLaputa.isChecked()) {
					deleteApplication("com.htc.laputa");
					deleteApplication("com.htc.laputa.HtcLaputaInstaller");
					deleteApplication("com.htc.laputa.widget3d.locations");
					deleteApplication("com.htc.laputa.widget3d.navigate");
					deleteApplication("com.htc.laputa.trip.TripWidget");
				}
				if (itmFlickr.isChecked()) {
					deleteApplication("com.htc.socialnetwork.flickr");
					deleteApplication("com.htc.engine.flickr");
				}
				if (itmFriendStream.isChecked()) {
					deleteApplication("com.htc.friendstream");
					deleteApplication("com.htc.FriendStream3DWidget");
					deleteApplication("com.htc.idlescreen.socialnetwork");
				}
				if (itmGoogle.isChecked()) {
					deleteApplication("com.google.android.apps.plus");
					deleteApplication("com.google.android.youtube");
					deleteApplication("com.htc.picasa");
					deleteApplication("com.google.android.gm");
					deleteApplication("com.google.android.voicesearch");
					deleteApplication("com.google.android.apps.genie.geniewidget");
				}
				if (itm3rd.isChecked()) {
					deleteApplication("com.adobe.flashplayer");
					deleteApplication("com.adobe.reader");
					deleteApplication("com.htc.pdfviewer");
					deleteApplication("com.infraware.docmaster");
					deleteApplication("com.htc.android.teeter");
				}

				h.sendEmptyMessage(1);

			}
		}).start();
	}

	private void deleteApplication(String namesapce) {
		try {
			ApplicationInfo info = GlobalInstance.pm.getApplicationInfo(
					namesapce, 0);
			String path = info.sourceDir;
			if (info.sourceDir.contains("/system/app/")) {
				ApkUtils.deleteSystemApp(path);
				ApkUtils.deleteSystemAppData(info.dataDir);
			} else {
				ApkUtils.uninstallApk(namesapce);
			}
		} catch (NameNotFoundException e) {
			Log.e("PackageNotFound", e.getMessage());
		}
	}

	@Override
	public Fragment replaceFragment() {
		// TODO Auto-generated method stub
		return null;
	}
}
