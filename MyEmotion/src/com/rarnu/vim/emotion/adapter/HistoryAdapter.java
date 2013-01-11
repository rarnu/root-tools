package com.rarnu.vim.emotion.adapter;

import java.util.List;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.vim.emotion.R;
import com.rarnu.vim.emotion.database.EmotionInfo;
import com.rarnu.vim.emotion.utils.MiscUtils;

public class HistoryAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private List<EmotionInfo> list;
	private Context context;

	public HistoryAdapter(Context context, List<EmotionInfo> list) {
		this.context = context;
		this.inflater = LayoutInflater.from(context);
		this.list = list;
	}

	public void setNewData(List<EmotionInfo> list) {
		this.list = list;
		this.notifyDataSetChanged();
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
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_history, parent, false);
		}
		HistoryHolder holder = (HistoryHolder) v.getTag();
		if (holder == null) {
			holder = new HistoryHolder();
			holder.tvDate = (TextView) v.findViewById(R.id.tvDate);
			holder.ivEmotion = (ImageView) v.findViewById(R.id.ivEmotion);
		}
		EmotionInfo item = list.get(position);
		if (item != null) {
			holder.tvDate.setText(String.format("%d.%d.%d", item.year,
					item.month + 1, item.day));
			holder.ivEmotion.setImageDrawable(MiscUtils.getBitmapByAssets(
					context, "face/" + item.emotion));
		}
		return v;
	}

}
