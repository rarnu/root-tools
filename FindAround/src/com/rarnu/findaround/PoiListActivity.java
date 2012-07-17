package com.rarnu.findaround;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;

public class PoiListActivity extends Activity implements OnClickListener, OnItemClickListener {

	ListView lvPoi;
	TextView tvName;
	Button btnLeft;
	
	PoiAdapter adapter;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.poi_list);
		
		lvPoi = (ListView) findViewById(R.id.lvPoi);
		tvName = (TextView) findViewById(R.id.tvName);
		btnLeft = (Button) findViewById(R.id.btnLeft);
		btnLeft.setText(R.string.back);
		btnLeft.setVisibility(View.VISIBLE);
		tvName.setText(R.string.list_result);
		btnLeft.setOnClickListener(this);
		lvPoi.setOnItemClickListener(this);
		
		adapter = new PoiAdapter(getLayoutInflater(), GlobalInstance.listPoi);
		lvPoi.setAdapter(adapter);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			setResult(RESULT_CANCELED);
			finish();
			break;
		}
		
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		GlobalInstance.selectedInfo = GlobalInstance.listPoi.get(position);
		setResult(RESULT_OK);
		finish();
	}

}
