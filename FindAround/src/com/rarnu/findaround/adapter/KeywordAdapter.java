package com.rarnu.findaround.adapter;

import java.util.List;

import android.os.Handler;
import android.os.Message;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.TextView;

import com.rarnu.findaround.R;

public class KeywordAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private List<String> list;
	private Handler h;

	public KeywordAdapter(LayoutInflater inflater, List<String> list, Handler h) {
		this.inflater = inflater;
		this.list = list;
		this.h = h;
	}

	@Override
	public int getCount() {
		return list.size();
	}

	@Override
	public Object getItem(int position) {
		return list.get(position);
	}

	@Override
	public long getItemId(int position) {
		return position;
	}

	@Override
	public View getView(final int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.keyword_item, parent, false);
		}

		KeywordHolder holder = (KeywordHolder) v.getTag();
		if (holder == null) {
			holder = new KeywordHolder();
			holder.tvKeyword = (TextView) v.findViewById(R.id.tvKeyword);
			holder.btnDelete = (Button) v.findViewById(R.id.btnDelete);
			v.setTag(holder);
		}

		String item = list.get(position);
		if (item != null) {
			holder.tvKeyword.setText(item);
			holder.btnDelete.setOnClickListener(new OnClickListener() {

				@Override
				public void onClick(View v) {
					Message msg = new Message();
					msg.what = 1;
					msg.arg1 = position;
					h.sendMessage(msg);
				}
			});
		}

		return v;
	}

}
