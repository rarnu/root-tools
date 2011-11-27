package com.snda.root.hosts;

import java.io.IOException;

import com.snda.root.hosts.root.RootUtils;
import com.snda.root.hosts.utils.FileUtils;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;

public class EditHostsActivity extends Activity implements OnClickListener {

	EditText etEditHosts;
	Button btnEditSave;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.edit_hosts);

		etEditHosts = (EditText) findViewById(R.id.etEditHosts);
		btnEditSave = (Button) findViewById(R.id.btnEditSave);

		etEditHosts.setText(GlobalInstance.hostsText);

		btnEditSave.setOnClickListener(this);

	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnEditSave:
			String hostText = etEditHosts.getText().toString();
			try {
				FileUtils.rewriteFile(GlobalInstance.app_dir
						+ GlobalInstance.host_filename, hostText);
				boolean pushed = RootUtils.pushFileToSystem(
						GlobalInstance.app_dir + GlobalInstance.host_filename,
						GlobalInstance.sys_dir, GlobalInstance.host_filename);

				Toast.makeText(
						this,
						pushed ? R.string.c_savehostsok
								: R.string.c_savehostsfail, Toast.LENGTH_LONG)
						.show();

				setResult(RESULT_OK);
				finish();
			} catch (IOException e) {
				Toast.makeText(this, R.string.c_savehostsfail,
						Toast.LENGTH_LONG).show();
			}
			break;
		}

	}
}
