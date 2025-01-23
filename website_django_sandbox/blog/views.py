from django.shortcuts import render
# from django.http import HttpResponse

#dummy data
posts = [
    {'author': "Dylan Rohan",
     'title': "Blog post one",
     'content': 'blah',
     'date_posted': 'Jan 1st'
     },

    {'author': "Dylan Rohan",
     'title': "Blog post two",
     'content': 'blah blah',
     'date_posted': 'Jan 2st'}
]
def blog_home_page(request):
    context = {
        'posts':posts
    }
    return render(request, 'blog/home.html', context)

def about_page(request):
    return render(request, 'blog/about.html', {'title': 'About'})