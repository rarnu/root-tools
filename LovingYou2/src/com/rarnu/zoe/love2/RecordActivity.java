package com.rarnu.zoe.love2;

import java.io.File;
import java.io.IOException;

import android.content.Intent;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.net.NetworkInfo;
import android.net.Uri;
import android.os.Bundle;
import android.provider.MediaStore;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.zoe.love2.api.LovingYouApi;
import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.common.Config;
import com.rarnu.zoe.love2.common.DayInfo;
import com.rarnu.zoe.love2.comp.Checker;
import com.rarnu.zoe.love2.comp.Title;
import com.rarnu.zoe.love2.utils.DownloadUtils;
import com.rarnu.zoe.love2.utils.MiscUtils;
import com.rarnu.zoe.love2.utils.NetworkUtils;
import com.rarnu.zoe.love2.utils.UIUtils;
import com.rarnu.zoe.love2.utils.WeiboUtils;
import com.weibo.sdk.android.WeiboException;
import com.weibo.sdk.android.net.RequestListener;

public class RecordActivity extends BaseActivity implements OnClickListener {

	RelativeLayout layLines, layBottomLine;
	// String[] text = null;
	ImageView imgBall;
	Checker chkE1, chkE2, chkE3, chkE4;
	Button btnSubmit;
	EditText etRecord;

	ImageView imgPhoto;
	TextView tvAddPicture;

	String photoFileName = "";
	File fTmp, fPhotoTmp = null;

	ImageView[] imgLines = new ImageView[21];

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		fTmp = new File(DownloadUtils.SAVE_PATH + "tmp.jpg");
		fPhotoTmp = new File(DownloadUtils.SAVE_PATH + "tmp_p.jpg");
		build21Lines(Global.database.getDay());

		etRecord.setText(Config.getLastText(this));
	}

	@Override
	protected void setContentView() {
		setContentView(R.layout.activity_record);

	}

	@Override
	protected void initComponents() {
		super.initComponents();

		title.getBarItem(Title.BARITEM_CENTER).setText(R.string.record_today);
		// title.getBarItem(Title.BARITEM_RIGHT)
		// .setIcon(R.drawable.record_history);
		title.getBarItem(Title.BARITEM_LEFT).setIcon(R.drawable.home);

		layLines = (RelativeLayout) findViewById(R.id.layLines);
		layBottomLine = (RelativeLayout) findViewById(R.id.layBottomLine);
		imgBall = (ImageView) findViewById(R.id.imgBall);

		chkE1 = (Checker) findViewById(R.id.chkE1);
		chkE2 = (Checker) findViewById(R.id.chkE2);
		chkE3 = (Checker) findViewById(R.id.chkE3);
		chkE4 = (Checker) findViewById(R.id.chkE4);

		btnSubmit = (Button) findViewById(R.id.btnSubmit);
		etRecord = (EditText) findViewById(R.id.etRecord);

		imgPhoto = (ImageView) findViewById(R.id.imgPhoto);
		tvAddPicture = (TextView) findViewById(R.id.tvAddPicture);

		resize();
		initEmotions();
	}

	private void resize() {
		int width = UIUtils.getWidth() - UIUtils.dipToPx(64)
				- (UIUtils.dipToPx(52) * 4);
		width = width / 3;
		RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) chkE2
				.getLayoutParams();
		rlp.leftMargin = width;
		chkE2.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) chkE3.getLayoutParams();
		rlp.leftMargin = width;
		chkE3.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) chkE4.getLayoutParams();
		rlp.leftMargin = width;
		chkE4.setLayoutParams(rlp);

	}

	private void initEmotions() {

		chkE1.setText(R.string.record_e6);
		chkE2.setText(R.string.record_e3);
		chkE3.setText(R.string.record_e2);
		chkE4.setText(R.string.record_e4);

		chkE1.setYesDrawable(R.drawable.record_e1y);
		chkE1.setNoDrawable(R.drawable.record_e1n);
		chkE2.setYesDrawable(R.drawable.record_e2y);
		chkE2.setNoDrawable(R.drawable.record_e2n);
		chkE3.setYesDrawable(R.drawable.record_e3y);
		chkE3.setNoDrawable(R.drawable.record_e3n);
		chkE4.setYesDrawable(R.drawable.record_e4y);
		chkE4.setNoDrawable(R.drawable.record_e4n);

		chkE1.setStatus(Checker.STATUS_NO);
		chkE2.setStatus(Checker.STATUS_NO);
		chkE3.setStatus(Checker.STATUS_NO);
		chkE4.setStatus(Checker.STATUS_NO);
	}

	@Override
	protected void initEvents() {
		super.initEvents();
		title.getBarItem(Title.BARITEM_LEFT).setOnButtonClick(this);
		// title.getBarItem(Title.BARITEM_RIGHT).setOnButtonClick(this);

		chkE1.setOnButtonClick(this);
		chkE2.setOnButtonClick(this);
		chkE3.setOnButtonClick(this);
		chkE4.setOnButtonClick(this);

		btnSubmit.setOnClickListener(this);
		imgPhoto.setOnClickListener(this);
		tvAddPicture.setOnClickListener(this);
	}

	@Override
	protected void onPause() {
		MiscUtils.hideInput(this);
		super.onPause();
	}

	@Override
	protected void onDestroy() {
		Config.setLastText(this, etRecord.getText().toString());

		super.onDestroy();
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case Title.ITEM_LEFT:
			LovingYouApi.saveLog(this, "RecordActivity", "Back");
			finish();
			break;
		// case Title.ITEM_RIGHT:
		// Intent inHistory = new Intent(this, HistoryActivity.class);
		// startActivity(inHistory);
		// break;
		case R.id.chkE1:
		case R.id.chkE2:
		case R.id.chkE3:
		case R.id.chkE4:
			LovingYouApi.saveLog(this, "RecordActivity", "ChangeStatus");
			changeCheckerStatus((Checker) v);
			break;
		case R.id.imgPhoto:
		case R.id.tvAddPicture:
			LovingYouApi.saveLog(this, "RecordActivity", "AddPicture");
			if (photoFileName.equals("")) {
				Intent inMethod = new Intent(this, PhotoMethodActivity.class);
				startActivityForResult(inMethod, 1);
			} else {
				photoFileName = "";
				imgPhoto.setImageBitmap(null);
				tvAddPicture.setText(R.string.send_picture);
			}
			break;
		case R.id.btnSubmit:
			LovingYouApi.saveLog(this, "RecordActivity", "Submit");
			NetworkInfo nInfo = NetworkUtils.getNetworkInfo(this);
			if (nInfo == null || !nInfo.isConnected()) {
				Toast.makeText(this, R.string.no_connection, Toast.LENGTH_LONG)
						.show();
				return;
			}

			// must write something
			String txt = etRecord.getText().toString();
			if (txt.equals("")) {
				Toast.makeText(this, R.string.record_hint, Toast.LENGTH_LONG)
						.show();
				return;
			}
			if (Config.TOKEN.equals("") || Config.EXPRIED == 0) {
				Toast.makeText(this, R.string.token_error, Toast.LENGTH_LONG)
						.show();
				return;
			}
			if (txt.length() > 140) {
				txt = txt.substring(0, 140);
			}

			long stamp = System.currentTimeMillis();
			int news = chkE1.getStatus() == Checker.STATUS_YES ? 0 : 1;
			int food = chkE2.getStatus() == Checker.STATUS_YES ? 0 : 1;
			int active = chkE3.getStatus() == Checker.STATUS_YES ? 0 : 1;
			int reading = chkE4.getStatus() == Checker.STATUS_YES ? 0 : 1;
			DayInfo info = Global.database.queryDay(Global.database.getDay());
			if ((news + food + active + reading) == 4) {
				if (info.day == -1) {
					Global.database.insertDay(stamp, 0, active, food, reading,
							news);
				}
			} else {
				Global.database
						.insertDay(stamp, 0, active, food, reading, news);
			}

			// photo?
			// Global.database.insertGround(Global.database.getDay(), txt, "");
			// Global.database.updateDay(Global.database.getDay(), 0);

			WeiboUtils.shareArticleToSina(txt, photoFileName,
					new RequestListener() {

						@Override
						public void onIOException(IOException arg0) {
							Log.e("ioexception", arg0.getMessage());

						}

						@Override
						public void onError(WeiboException arg0) {
							Log.e("error", arg0.getMessage());

						}

						@Override
						public void onComplete(String arg0) {
							Log.e("complete", arg0);

						}
					});

			Intent inHis = new Intent(this, HistoryActivity.class);
			startActivity(inHis);
			etRecord.setText("");
			finish();
			break;

		}
	}

	private void doChoosePhoto() {
		Intent intent = new Intent(Intent.ACTION_GET_CONTENT, null);
		intent.setType("image/*");
		intent.putExtra("crop", "circle");
		intent.putExtra("noFaceDetection", true);
		if (fTmp.exists()) {
			fTmp.delete();
		}
		intent.putExtra("output", Uri.fromFile(fTmp));
		intent.putExtra("outputFormat", "JPEG");
		startActivityForResult(intent, 0);
	}

	private void doCropPhoto(Uri uri) {
		Intent intent = new Intent("com.android.camera.action.CROP");
		intent.setDataAndType(uri, "image/*");
		intent.putExtra("crop", "true");
		if (fTmp.exists()) {
			fTmp.delete();
		}
		intent.putExtra("output", Uri.fromFile(fTmp));
		intent.putExtra("outputFormat", "JPEG");
		startActivityForResult(intent, 0);
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode != RESULT_OK) {
			return;
		}

		switch (requestCode) {
		case 0:
			if (fTmp.exists()) {
				photoFileName = fTmp.getAbsolutePath();

				BitmapFactory.Options bop = new BitmapFactory.Options();
				bop.inSampleSize = 4;
				imgPhoto.setImageBitmap(BitmapFactory.decodeFile(photoFileName,
						bop));
				tvAddPicture.setText(R.string.remove_photo);
			}
			break;
		case 1:
			int method = data.getIntExtra("method", 0);
			if (method == 0) {
				Intent intent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
				if (fPhotoTmp.exists()) {
					fPhotoTmp.delete();
				}
				intent.putExtra(MediaStore.EXTRA_OUTPUT,
						Uri.fromFile(fPhotoTmp));
				startActivityForResult(intent, 2);
			} else {
				doChoosePhoto();
			}
			break;
		case 2:
			doCropPhoto(Uri.fromFile(fPhotoTmp));
			break;
		}
	}

	private void changeCheckerStatus(Checker chk) {
		chk.setStatus(chk.getStatus() == Checker.STATUS_YES ? Checker.STATUS_NO
				: Checker.STATUS_YES);
	}

	private void build21Lines(int day) {

		int width = UIUtils.getWidth() / 21;
		int ballLeft = 0;
		for (int i = 0; i < 21; i++) {
			imgLines[i] = new ImageView(this);
			RelativeLayout.LayoutParams rlp = new RelativeLayout.LayoutParams(
					LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
			rlp.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM, RelativeLayout.TRUE);
			if ((i + 1) % 5 == 0) {
				imgLines[i].setBackgroundResource(R.drawable.record_bl);
				rlp.height = UIUtils.dipToPx(12);
				rlp.leftMargin = (width * (i + 1)) - UIUtils.dipToPx(2);
				ballLeft = rlp.leftMargin - UIUtils.dipToPx(4);
				TextView tvNumber = new TextView(this);
				RelativeLayout.LayoutParams trlp = new RelativeLayout.LayoutParams(
						LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
				trlp.leftMargin = rlp.leftMargin
						- UIUtils.dipToPx(i < 5 ? 1 : 3);
				trlp.addRule(RelativeLayout.CENTER_VERTICAL,
						RelativeLayout.TRUE);
				tvNumber.setLayoutParams(trlp);
				tvNumber.setTextColor(Color.BLACK);
				tvNumber.setTextSize(8);
				tvNumber.setText(String.valueOf(i + 1));
				layBottomLine.addView(tvNumber);
			} else {
				imgLines[i].setBackgroundResource(R.drawable.record_sl);
				rlp.height = UIUtils.dipToPx(8);
				rlp.leftMargin = (width * (i + 1)) - UIUtils.dipToPx(1);
				ballLeft = rlp.leftMargin - UIUtils.dipToPx(5);
			}
			imgLines[i].setLayoutParams(rlp);
			layLines.addView(imgLines[i]);
			if ((i + 1) == day) {
				moveBall(ballLeft);
			}
		}
	}

	private void moveBall(int marginLeft) {
		RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) imgBall
				.getLayoutParams();
		rlp.leftMargin = marginLeft;
		imgBall.setLayoutParams(rlp);
	}
}
