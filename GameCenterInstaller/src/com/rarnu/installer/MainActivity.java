package com.rarnu.installer;

import android.app.Activity;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.Toast;

public class MainActivity extends Activity implements OnClickListener {

	Button btnInstall;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);

		btnInstall = (Button) findViewById(R.id.btnInstall);
		btnInstall.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		doInstallGameCenter();

	}

	private void doInstallGameCenter() {
		btnInstall.setText(R.string.installing);
		btnInstall.setEnabled(false);

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					btnInstall.setText(R.string.install);
					btnInstall.setEnabled(true);
					Toast.makeText(MainActivity.this,
							String.format("Return Code: %d", msg.arg1),
							Toast.LENGTH_LONG).show();
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				int ret = GameCenterInstaller.install(MainActivity.this);
				Message msg = new Message();
				msg.what = 1;
				msg.arg1 = ret;
				h.sendMessage(msg);

			}
		}).start();
	}

}
