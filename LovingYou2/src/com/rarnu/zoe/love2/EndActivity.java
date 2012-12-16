package com.rarnu.zoe.love2;

import java.util.List;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;

import com.rarnu.zoe.love2.api.LovingYouApi;
import com.rarnu.zoe.love2.common.DayInfo;
import com.rarnu.zoe.love2.utils.MiscUtils;

public class EndActivity extends Activity implements OnClickListener {

	ImageView btnStart;
	ImageView img1, img2, img3;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_splash);
		img1 = (ImageView) findViewById(R.id.img1);
		img2 = (ImageView) findViewById(R.id.img2);
		img3 = (ImageView) findViewById(R.id.img3);

		showResult();

		btnStart = (ImageView) findViewById(R.id.btnStart);
		btnStart.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		shareTo();
	}

	private void showResult() {
		List<DayInfo> listInfo = Global.database.queryFullHistory();
		int task = 0;
		for (int i = 0; i < listInfo.size(); i++) {
			task += (listInfo.get(i).task == 0 ? 1 : 0);
		}
		setImages(task >= 21);
	}

	private void setImages(boolean succ) {
		// set images
		img1.setImageResource(succ ? R.drawable.succ1 : R.drawable.fail1);
		img2.setImageResource(succ ? R.drawable.succ2 : R.drawable.fail2);
		img3.setImageResource(succ ? R.drawable.succ3 : R.drawable.fail3);
	}

	private void shareTo() {
		LovingYouApi.saveLog(this, "EndActivity", "Share");
		Intent intent = new Intent(Intent.ACTION_SEND);
		intent.setType("image/*");
		intent.putExtra(Intent.EXTRA_SUBJECT, getString(R.string.share_title));
		intent.putExtra(Intent.EXTRA_TEXT, getString(R.string.share_text));
		intent.putExtra(Intent.EXTRA_STREAM, MiscUtils.saveLocalFile(this, 0));
		startActivityForResult(Intent.createChooser(intent, getString(R.string.share)), 0);

	}
	
	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		finish();
	}

}
