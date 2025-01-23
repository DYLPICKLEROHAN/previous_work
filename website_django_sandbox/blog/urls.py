from django.urls import path
from.import views

urlpatterns = [
    path("", views.blog_home_page, name = 'blog-home'),
    path("about/", views.about_page, name = 'blog-about')
]
