package com.snda.root.d23h;

import java.io.File;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;
import android.widget.RelativeLayout.LayoutParams;

import com.snda.root.d23h.root.RootUtils;

public class MainActivity extends Activity implements OnClickListener {

	static int USER_ID = 100;
	static int USER_ID2 = 200;

	static String cpCmd = "cp /data/data/com.gameloft.android.GAND.GloftD23H/dh2_00%d.savegame /sdcard/gameloft/save/GloftD23H/%s/";
	static String cpRevCmd = "cp /sdcard/gameloft/save/GloftD23H/%s/dh2_00%d.savegame /data/data/com.gameloft.android.GAND.GloftD23H/";
	static String chmodCmd = "chmod 777 /data/data/com.gameloft.android.GAND.GloftD23H/dh2_00%d.savegame";
	static String rmCmd = "rm /data/data/com.gameloft.android.GAND.GloftD23H/*.bak";
	static String rmCheckpoint = "rm /data/data/com.gameloft.android.GAND.GloftD23H/dh2_00*.checkpoint";
	static String rmLevel = "rm /data/data/com.gameloft.android.GAND.GloftD23H/dh2_00*_*_level.savegame";
	static String rmCurrentCmd = "rm /sdcard/gameloft/save/GloftD23H/%s/current";
	static String rmTouchCmd = "touch /sdcard/gameloft/save/GloftD23H/%s/current";

	RelativeLayout lay;
	TextView tvSave;
	Button btnCleanData, btnResetData;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);

		lay = (RelativeLayout) findViewById(R.id.lay);
		tvSave = (TextView) findViewById(R.id.tvSave);
		btnCleanData = (Button) findViewById(R.id.btnCleanData);
		btnResetData = (Button) findViewById(R.id.btnResetData);

		buildButtons();

		btnCleanData.setOnClickListener(this);
	}

	@SuppressWarnings("deprecation")
	private void buildButtons() {
		File f = new File("/sdcard/gameloft/save/GloftD23H");
		if (!f.exists()) {
			return;
		}
		String[] dirs = f.list();

		for (int i = 0; i < dirs.length; i++) {
			File fSave = new File("/sdcard/gameloft/save/GloftD23H/" + dirs[i]
					+ "/current");
			if (fSave.exists()) {
				tvSave.setText(dirs[i]);
			}
			Button btnSave = new Button(this);
			btnSave.setId(USER_ID + i);
			RelativeLayout.LayoutParams rlp = new RelativeLayout.LayoutParams(
					LayoutParams.FILL_PARENT, 80);
			if (i > 0) {
				rlp.addRule(RelativeLayout.BELOW, USER_ID + i - 1);
			}
			rlp.bottomMargin = 8;
			btnSave.setLayoutParams(rlp);
			btnSave.setText(dirs[i]);
			btnSave.setOnClickListener(this);
			lay.addView(btnSave);

		}

	}

	@Override
	public void onClick(View v) {
		if (v.getId() == R.id.btnCleanData) {
			cleanData();
			Toast.makeText(this, "Clean Data finished", Toast.LENGTH_LONG)
					.show();
		} else if (v.getId() == R.id.btnResetData) {
			String current = tvSave.getText().toString();
			resetCurrent(current);
			Toast.makeText(this, "Reset data finished", Toast.LENGTH_LONG)
					.show();
		} else {
			String current = tvSave.getText().toString();
			String dir = ((Button) v).getText().toString();

			backupCurrentSave(current);
			if (current.equals(dir)) {
				Toast.makeText(this, "Backup data finished", Toast.LENGTH_LONG)
						.show();
				return;
			}
			switchSave(current, dir);
			Toast.makeText(this, "Switch Save OK", Toast.LENGTH_LONG).show();

		}

	}

	private void backupCurrentSave(String current) {
		RootUtils.runRootCommand(String.format(cpCmd, 0, current));
		RootUtils.runRootCommand(String.format(cpCmd, 1, current));
		RootUtils.runRootCommand(String.format(cpCmd, 2, current));
	}

	private void switchSave(String current, String dir) {
		RootUtils.runRootCommand(String.format(rmCurrentCmd, current));
		cleanData();
		RootUtils.runRootCommand(String.format(cpRevCmd, dir, 0));
		RootUtils.runRootCommand(String.format(cpRevCmd, dir, 1));
		RootUtils.runRootCommand(String.format(cpRevCmd, dir, 2));
		RootUtils.runRootCommand(String.format(chmodCmd, 0));
		RootUtils.runRootCommand(String.format(chmodCmd, 1));
		RootUtils.runRootCommand(String.format(chmodCmd, 2));
		RootUtils.runRootCommand(String.format(rmTouchCmd, dir));
		tvSave.setText(dir);
	}

	private void resetCurrent(String current) {
		cleanData();
		RootUtils.runRootCommand(String.format(cpRevCmd, current, 0));
		RootUtils.runRootCommand(String.format(cpRevCmd, current, 1));
		RootUtils.runRootCommand(String.format(cpRevCmd, current, 2));
		RootUtils.runRootCommand(String.format(chmodCmd, 0));
		RootUtils.runRootCommand(String.format(chmodCmd, 1));
		RootUtils.runRootCommand(String.format(chmodCmd, 2));
	}

	private void cleanData() {
		RootUtils.runRootCommand(rmCmd);
		RootUtils.runRootCommand(rmCheckpoint);
		RootUtils.runRootCommand(rmLevel);
	}
}