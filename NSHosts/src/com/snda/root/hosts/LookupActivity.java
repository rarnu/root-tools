package com.snda.root.hosts;

import android.app.Activity;
import android.os.Bundle;
import android.util.DisplayMetrics;
import android.widget.Button;
import android.widget.RelativeLayout;

public class LookupActivity extends Activity {

	Button btnLookup;
	Button btnCom, btnOrg, btnCn;
	
	DisplayMetrics dm = new DisplayMetrics();

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.nslookup);

		btnLookup = (Button) findViewById(R.id.btnLookup);
		btnCom = (Button) findViewById(R.id.btnCom);
		btnOrg = (Button) findViewById(R.id.btnOrg);
		btnCn = (Button) findViewById(R.id.btnCn);
		resizeCompleteButtons();
	}

	private void resizeCompleteButtons() {
		
		getWindowManager().getDefaultDisplay().getMetrics(dm);
		
		int w = getWindowManager().getDefaultDisplay().getWidth();

		RelativeLayout.LayoutParams lp = (RelativeLayout.LayoutParams) btnLookup
				.getLayoutParams();
		int btnw = (w - lp.width - dipToPx(8)) / 3;

		lp = (RelativeLayout.LayoutParams) btnCom.getLayoutParams();
		lp.width = btnw;
		btnCom.setLayoutParams(lp);

		lp = (RelativeLayout.LayoutParams) btnOrg.getLayoutParams();
		lp.width = btnw;
		btnOrg.setLayoutParams(lp);

		lp = (RelativeLayout.LayoutParams) btnCn.getLayoutParams();
		lp.width = btnw;
		btnCn.setLayoutParams(lp);

	}
	
	public int dipToPx(int dip) {
		return (int) (dip * dm.density + 0.5f);
	}
}
