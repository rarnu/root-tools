package com.snda.gyue;

import android.app.Activity;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.Window;
import android.widget.Button;
import android.widget.EditText;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import com.snda.gyue.classes.ArticleItem;
import com.snda.gyue.utils.ShareUtils;

public class ShareEditActivity extends Activity implements OnClickListener {

	String to = "";
	EditText etEdit;
	Button btnOk, btnCancel;
	RelativeLayout laySharing;
	TextView tvShare;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		setContentView(R.layout.share_edit);

		to = getIntent().getStringExtra("to");
		if (to == null || to.equals("")) {
			Toast.makeText(this, R.string.error, Toast.LENGTH_LONG).show();
			finish();
			return;
		}

		etEdit = (EditText) findViewById(R.id.etEdit);
		btnOk = (Button) findViewById(R.id.btnOK);
		btnCancel = (Button) findViewById(R.id.btnCancel);
		laySharing = (RelativeLayout) findViewById(R.id.laySharing);
		tvShare = (TextView) findViewById(R.id.tvShare);

		if (to.equals("sina")) {
			tvShare.setText(R.string.share_sina);
		} else {
			tvShare.setText(R.string.share_tencent);
		}

		ArticleItem item = GlobalInstance.currentArticle;

		String txt = "《" + item.getTitle() + "》 " + item.getDescription();
		txt = txt.substring(0, 100);
		txt += item.getLink();

		etEdit.setText(txt);

		btnOk.setOnClickListener(this);
		btnCancel.setOnClickListener(this);

	}

	private void shareToSinaT() {
		etEdit.setEnabled(false);
		btnOk.setEnabled(false);
		btnCancel.setEnabled(false);
		laySharing.setVisibility(View.VISIBLE);

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					Toast.makeText(ShareEditActivity.this,
							(msg.arg1 == 1 ? R.string.share_sina_ok : R.string.share_sina_fail), Toast.LENGTH_LONG)
							.show();

					laySharing.setVisibility(View.GONE);
					etEdit.setEnabled(true);
					btnOk.setEnabled(true);
					btnCancel.setEnabled(true);
					finish();
				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				boolean bSina = ShareUtils.shareArticleToSina(GlobalInstance.currentArticle, etEdit.getText()
						.toString());
				Message msg = new Message();
				msg.what = 1;
				msg.arg1 = (bSina ? 1 : 0);
				h.sendMessage(msg);
			}
		}).start();
	}

	private void shareToTencentT() {
		etEdit.setEnabled(false);
		btnOk.setEnabled(false);
		btnCancel.setEnabled(false);
		laySharing.setVisibility(View.VISIBLE);
		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					Toast.makeText(ShareEditActivity.this,
							(msg.arg1 == 1 ? R.string.share_tencent_ok : R.string.share_tencent_fail),
							Toast.LENGTH_LONG).show();
					laySharing.setVisibility(View.GONE);
					etEdit.setEnabled(true);
					btnOk.setEnabled(true);
					btnCancel.setEnabled(true);
					finish();
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				boolean bTencent = ShareUtils.shareArticleToTencent(GlobalInstance.currentArticle, etEdit.getText()
						.toString());
				Message msg = new Message();
				msg.what = 1;
				msg.arg1 = (bTencent ? 1 : 0);
				h.sendMessage(msg);
			}
		}).start();
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnCancel:
			finish();
			break;
		case R.id.btnOK:
			if (to.equals("sina")) {
				shareToSinaT();
			} else {
				shareToTencentT();
			}
			break;
		}

	}
}
