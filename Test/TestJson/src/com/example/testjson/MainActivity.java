package com.example.testjson;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.widget.TextView;

import com.rarnu.utils.FileUtils;
import com.rarnu.utils.JsonUtils;
import com.rarnu.utils.common.JsonNode;
import com.rarnu.utils.common.JsonNode.FieldType;

public class MainActivity extends Activity {

	TextView tvJson;
	TestClass tc;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);
		tvJson = (TextView) findViewById(R.id.tvJson);

		JsonNode root = new JsonNode("", FieldType.ftObject);
		root.childs.add(new JsonNode("fa", FieldType.ftValue));
		root.childs.add(new JsonNode("fb", FieldType.ftValue));

		JsonNode inner = new JsonNode("inner", FieldType.ftObject);
		inner.childs.add(new JsonNode("innerFA", FieldType.ftValue));
		inner.childs.add(new JsonNode("innerFB", FieldType.ftValue));

		JsonNode nodeListObj = new JsonNode("array", FieldType.ftObject);
		nodeListObj.childs.add(new JsonNode("arr", FieldType.ftValue));
		inner.childs.add(new JsonNode("array", FieldType.ftList, nodeListObj));

		inner.childs.add(new JsonNode("arrStr", FieldType.ftList, new JsonNode(
				"arrStr", FieldType.ftValue)));

		JsonNode nodeMapObj = new JsonNode("map", FieldType.ftValue);
		inner.childs.add(new JsonNode("map", FieldType.ftMap, nodeMapObj));

		root.childs.add(inner);
		JsonNode test = new JsonNode("test", FieldType.ftMap);
		test.subItemNode = new JsonNode("test", FieldType.ftObject);
		test.subItemNode.childs.add(new JsonNode("map", FieldType.ftValue));

		root.childs.add(test);

		JsonUtils<TestClass> ju = new JsonUtils<TestClass>(TestClass.class,
				root);
		tc = new TestClass();
		try {
			String jstr = ju.toJson(tc);
			tvJson.setText(jstr);
			Log.e("MainActivity1", jstr);
		} catch (Exception e) {
			Log.e("MainActivity1err", e.getMessage());
		}

		JsonUtils<TestClass> jc = new JsonUtils<TestClass>(TestClass.class,
				root);
		try {
			String jsonString = FileUtils.readAssetFile(this, "json");
			TestClass t = jc.toObject(jsonString);
			Log.e("MainActivity2", t.toString());
		} catch (Exception e) {
			Log.e("MainActivity2err", e.toString());
		}

	}

}
