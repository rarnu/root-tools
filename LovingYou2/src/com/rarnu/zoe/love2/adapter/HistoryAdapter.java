package com.rarnu.zoe.love2.adapter;

import java.util.List;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.zoe.love2.R;
import com.rarnu.zoe.loving.common.DataInfo;

public class HistoryAdapter extends BaseAdapter {

	// private Context context;
	private List<DataInfo> list;
	private LayoutInflater inflater;
	private int height;

	public HistoryAdapter(Context context, List<DataInfo> list, int height) {
//		this.context = context;
		this.list = list;
		this.inflater = LayoutInflater.from(context);
		this.height = height;
	}

	@Override
	public int getCount() {
		return list.size();
	}

	@Override
	public Object getItem(int arg0) {
		return list.get(arg0);
	}

	@Override
	public long getItemId(int arg0) {
		return arg0;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_history, parent, false);
		}
		HistoryHolder holder = (HistoryHolder) v.getTag();
		if (holder == null) {
			holder = new HistoryHolder();
			holder.imgHistory = (ImageView) v.findViewById(R.id.imgHistory);
			holder.tvHistory = (TextView) v.findViewById(R.id.tvHistory);
			AbsListView.LayoutParams rlp = (AbsListView.LayoutParams) v
					.getLayoutParams();
			rlp.height = height;
			v.setLayoutParams(rlp);

			v.setTag(holder);
		}

		DataInfo item = list.get(position);
		if (item != null) {
			holder.imgHistory.setVisibility(item.data < 3 ? View.VISIBLE
					: View.GONE);
			holder.tvHistory.setVisibility(item.data < 3 ? View.GONE
					: View.VISIBLE);
			holder.tvHistory.setText(item.data == 99 ? "" : "X");
		}

		return v;
	}

}
