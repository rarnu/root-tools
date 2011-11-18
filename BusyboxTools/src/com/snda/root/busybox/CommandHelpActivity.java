package com.snda.root.busybox;

import com.snda.root.busybox.utils.BusyboxUtils;

import android.app.Activity;
import android.os.Bundle;
import android.widget.TextView;

public class CommandHelpActivity extends Activity {

	TextView tvCommand;
	TextView tvHelp;
	
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.commandhelp);
		
		tvCommand = (TextView) findViewById(R.id.tvCommand);
		tvHelp = (TextView) findViewById(R.id.tvHelp);
		
		String cmd = getIntent().getStringExtra("cmd");
		
		setHelpContext(cmd);
	}
	
	private void setHelpContext(String cmd) {
		tvCommand.setText(cmd);
		String help = BusyboxUtils.getCommandHelp(cmd);
		tvHelp.setText(help);
	}
}
