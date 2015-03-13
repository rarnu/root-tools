package com.zoe.calendar.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import com.rarnu.devlib.adapter.BaseAdapter;
import com.zoe.calendar.R;

public class TagsTypeAdapter extends BaseAdapter<String> {

	public TagsTypeAdapter(Context context, List<String> list) {
		super(context, list);
	}

	@Override
	public View getView(final int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_tags_type, parent, false);
		}
		TagsTypeHolder holder = (TagsTypeHolder) v.getTag();
		if (holder == null) {
			holder = new TagsTypeHolder();
			holder.tvTitle = (TextView) v.findViewById(R.id.tvTitle);
			v.setTag(holder);
		}
		String item = list.get(position);
		if (item != null) {
			holder.tvTitle.setText(item);
		}
		return v;
	}

	@Override
	public String getValueText(String item) {
		return "";
	}

}
