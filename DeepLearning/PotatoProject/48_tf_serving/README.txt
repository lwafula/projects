
PS C:\Users\PC> docker run -it -v C:\Users\Public\lmaaya\Projects\DeepLearning\PotatoProject:/potato_tf_serving -p 8686:8686 --entrypoint /bin/bash tensorflow/serving


Using port 8601 (this can change)
de

PS C:\Users\PC> docker run -it -v C:\Users\Public\lmaaya\Projects\DeepLearning\PotatoProject\48_tf_serving:/48_tf_serving -p 8600:8600 --entrypoint /bin/bash tensorflow/serving


    requests.post(endpoint, json= json_data)
    predictions = MODEL.predict(img_batch)
    predicted_class = CLASS_NAMES[np.argmax(predictions[0])]
    confidence = np.max(predictions)

    return {
        'class' : predicted_class,
        'confidence': float(confidence)
    }


MODEL = tf.keras.models.load_model("../saved_models/1")
CLASS_NAMES = ["Early Blight", "Late Blight", "Healthy"]
